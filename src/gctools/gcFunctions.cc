/* -^- */
int gcFunctions_top;
#include <clasp/core/foundation.h>

#include <boost/mpl/list.hpp>
#ifdef USE_BOEHM
#include <gc/gc_mark.h>
#endif
int gcFunctions_before;
#ifdef USE_MPS
extern "C" {
#include <clasp/mps/code/mpscamc.h>
};
#endif

int gcFunctions_after;

#include <stdint.h>
#include <execinfo.h>
#include <unistd.h>
#include <sstream>
#include <iomanip>

#include <clasp/core/object.h>
#include <clasp/core/bformat.h>
#include <clasp/core/lisp.h>
#include <clasp/core/instance.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/environment.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/gctools/gc_interface.h>
#include <clasp/gctools/threadlocal.h>
#include <clasp/core/wrappers.h>

#ifdef DEBUG_TRACK_UNWINDS
std::atomic<size_t> global_unwind_count;
std::atomic<size_t> global_ReturnFrom_count;
std::atomic<size_t> global_DynamicGo_count;
std::atomic<size_t> global_CatchThrow_count;
#endif


extern "C" {

#ifdef DEBUG_FLOW_TRACKER
FILE* global_flow_tracker_file;

mp::Mutex global_flow_tracker_mutex;
size_t global_flow_tracker_counter = 0;
bool global_flow_tracker_on = false;
#define FLOW_TRACKER_LAST_THROW_BACKTRACE_SIZE 4096
void* global_flow_tracker_last_throw_backtrace[FLOW_TRACKER_LAST_THROW_BACKTRACE_SIZE];
Fixnum global_flow_tracker_last_throw_tracker_counter = 0;
size_t global_flow_tracker_last_throw_backtrace_size;
void initialize_flow_tracker()
{
  stringstream ss;
  ss << "/tmp/flowtracker-" << getpid() << ".dat";
  global_flow_tracker_file = fopen(ss.str().c_str(),"w");
  size_t stack_size = MAX_STACK_SIZE_FLOW_TRACKER;
  fwrite(&stack_size,sizeof(size_t),1,global_flow_tracker_file);
  global_flow_tracker_counter = 0;
  global_flow_tracker_on = false;
};

void flow_tracker_about_to_throw(Fixnum tracker_counter) {
  global_flow_tracker_last_throw_tracker_counter = tracker_counter;
  global_flow_tracker_last_throw_backtrace_size = backtrace(global_flow_tracker_last_throw_backtrace,FLOW_TRACKER_LAST_THROW_BACKTRACE_SIZE);
}

void flow_tracker_last_throw_backtrace_dump() {
  if (!global_flow_tracker_on) {
    printf("!!!!! Turn the flow-tracker on   Use: (gctools:flow-tracker-on)\n");
  }
  printf("flow_tracker_last_throw_backtrace_dump %lu frames\n", global_flow_tracker_last_throw_backtrace_size);
  printf("global_flow_tracker_last_throw_tracker_counter -> %lld\n", global_flow_tracker_last_throw_tracker_counter );
  for ( int i=0; i<global_flow_tracker_last_throw_backtrace_size; ++i ) {
    printf("dis -s %p\n", global_flow_tracker_last_throw_backtrace[i]);
  }
  fflush(stdout);
}


size_t next_flow_tracker_counter() {
  size_t result = 0;
  if (global_flow_tracker_on) {
    global_flow_tracker_mutex.lock();
    result = global_flow_tracker_counter++;
    fwrite(&result,sizeof(size_t),1,global_flow_tracker_file);
    void* buffer[MAX_STACK_SIZE_FLOW_TRACKER];
    backtrace(buffer,MAX_STACK_SIZE_FLOW_TRACKER);
    fwrite(buffer,sizeof(void*),MAX_STACK_SIZE_FLOW_TRACKER,global_flow_tracker_file);
    global_flow_tracker_mutex.unlock();
  }
  return result;
}

void flow_tracker_flush() {
  fflush(global_flow_tracker_file);
}

void flow_tracker_close() {
  fclose(global_flow_tracker_file);
}
#endif

};


namespace gctools {
std::atomic<double>   global_DiscriminatingFunctionCompilationSeconds = ATOMIC_VAR_INIT(0.0);

CL_DEFUN void gctools__accumulate_discriminating_function_compilation_seconds(double seconds) {
  global_DiscriminatingFunctionCompilationSeconds =
      global_DiscriminatingFunctionCompilationSeconds + seconds;
}

CL_DEFUN double gctools__discriminating_function_compilation_seconds() {
  return global_DiscriminatingFunctionCompilationSeconds;
}

};


namespace gctools {


size_t global_next_unused_kind = STAMP_max+1;

/*! Hardcode a few kinds of objects for bootstrapping
 */

const char *global_HardcodedKinds[] = {
    "", "core::T_O", "core::Instance_O", "core::Symbol_O"};

CL_DEFUN int gctools__max_bootstrap_kinds() {
  return sizeof(global_HardcodedKinds) / sizeof(global_HardcodedKinds[0]);
}

int iBootstrapKind(const string &name) {
  for (int i(0), iEnd(gctools__max_bootstrap_kinds()); i < iEnd; ++i) {
    //            printf("%s:%d i[%d]Checking if %s == %s\n", __FILE__, __LINE__, i, global_HardcodedKinds[i], name.c_str());
    if (strcmp(global_HardcodedKinds[i], name.c_str()) == 0) {
      return i;
    }
  }
  SIMPLE_ERROR(BF("Illegal bootstrap-kind %s") % name);
}

std::atomic<size_t> global_lexical_depth_counter;

CL_DEFUN core::T_sp gctools__next_lexical_depth_counter() {
  core::T_sp result = core::make_fixnum(++global_lexical_depth_counter);
  return result;
}

#ifdef DEBUG_FLOW_TRACKER
CL_DEFUN void gctools__flow_tracker_on() {
  initialize_flow_tracker();
  global_flow_tracker_on = true;
}

CL_DEFUN void gctools__flow_tracker_off() {
  global_flow_tracker_on = false;
  flow_tracker_close();
}
#endif

CL_DEFUN core::Cons_sp gctools__bootstrap_kind_symbols() {
  core::Cons_sp list(_Nil<core::Cons_O>());
  for (int i(gctools__max_bootstrap_kinds() - 1); i > 0; --i) {
    string name = global_HardcodedKinds[i];
    list = core::Cons_O::create(core::SimpleBaseString_O::make(name), list);
  }
  return list;
}

CL_DEFUN core::T_sp gctools__bootstrap_kind_p(const string &name) {
  for (int i(0), iEnd(gctools__max_bootstrap_kinds()); i < iEnd; ++i) {
    //            printf("%s:%d i[%d]Checking if %s == %s\n", __FILE__, __LINE__, i, global_HardcodedKinds[i], name.c_str());
    if (strcmp(global_HardcodedKinds[i], name.c_str()) == 0) {
      return core::make_fixnum(i);
    }
  }
  return _Nil<core::T_O>();
}

#ifdef DEBUG_TRACK_UNWINDS
CL_DEFUN size_t gctools__unwind_counter() {
  return global_unwind_count;
}
CL_DEFUN size_t gctools__dynamic_go_counter() {
  return global_DynamicGo_count;
}
CL_DEFUN size_t gctools__return_from_counter() {
  return global_ReturnFrom_count;
}
CL_DEFUN size_t gctools__catch_throw_counter() {
  return global_CatchThrow_count;
}
#endif

CL_DEFUN void gctools__change_sigchld_sigport_handlers()
{
  void* old = (void*)signal(SIGCHLD, SIG_DFL);
  signal(SIGPIPE, SIG_DFL);
  printf("%s:%d old signal handler for SIGCHLD = %p   SIG_DFL = %p\n", __FILE__, __LINE__, old, SIG_DFL);
}

CL_DEFUN core::T_sp gctools__unix_signal_handlers()
{
  return _lisp->_Roots._UnixSignalHandlers;
}

CL_DEFUN void gctools__deallocate_unmanaged_instance(core::T_sp obj) {
  obj_deallocate_unmanaged_instance(obj);
}

CL_DOCSTRING("Return bytes allocated (values clasp-calculated-bytes)");
CL_DEFUN core::T_sp gctools__bytes_allocated() {
  size_t my_bytes = my_thread_low_level->_Allocations._BytesAllocated;
  ASSERT(my_bytes < gc::most_positive_fixnum);
  return core::clasp_make_fixnum(my_bytes);
}



CL_DOCSTRING("Return the next unused kind");
CL_DEFUN size_t core__next_unused_kind() {
  size_t next = global_next_unused_kind;
  ++global_next_unused_kind;
  return next;
}

};


namespace gctools {

CL_DOCSTRING("Return the header value for the object");
CL_DEFUN core::T_sp core__header_value(core::T_sp obj) {
  if (obj.generalp()) {
    void *mostDerived = gctools::untag_general<void *>(obj.raw_());
    const gctools::Header_s *header = reinterpret_cast<const gctools::Header_s *>(gctools::ClientPtrToBasePtr(mostDerived));
    return core::clasp_make_integer(header->header._value);
  }
  SIMPLE_ERROR(BF("The object %s is not a general object and doesn't have a header-value") % _rep_(obj));
}


CL_DOCSTRING("Return the header value for the object");
CL_DEFUN core::T_sp core__header_value_to_stamp(core::T_sp value) {
  if (value.fixnump()) {
    Fixnum fvalue = value.unsafe_fixnum();
    return core::make_fixnum(Header_s::value_to_stamp(fvalue));
  }
  TYPE_ERROR(value,cl::_sym_fixnum);
}

CL_DEFUN core::T_mv gctools__tagged_pointer_mps_test()
{
  // Return the values used to identify tagged pointers (PTR&POINTER_TAG_MASK)==POINTER_TAG_EQ
  return Values(core::make_fixnum(POINTER_TAG_MASK),core::make_fixnum(POINTER_TAG_EQ));
}


CL_DOCSTRING("Return the header kind for the object");
CL_DEFUN Fixnum core__header_kind(core::T_sp obj) {
  if (obj.consp()) {
    return gctools::STAMP_CONS;
  } else if (obj.fixnump()) {
    return gctools::STAMP_FIXNUM;
  } else if (obj.generalp()) {
    void *mostDerived = gctools::untag_general<void *>(obj.raw_());
    const gctools::Header_s *header = reinterpret_cast<const gctools::Header_s *>(gctools::ClientPtrToBasePtr(mostDerived));
    gctools::GCStampEnum stamp = header->stamp();
    return (Fixnum)stamp;
  } else if (obj.single_floatp()) {
    return gctools::STAMP_SINGLE_FLOAT;
  } else if (obj.characterp()) {
    return gctools::STAMP_CHARACTER;
  } else if (obj.valistp()) {
    return gctools::STAMP_VA_LIST_S;
  }
  printf("%s:%d HEADER-KIND requested for a non-general object - Clasp needs to define hard-coded kinds for non-general objects - returning -1 for now", __FILE__, __LINE__);
  SIMPLE_ERROR(BF("The object %s doesn't have a stamp") % _rep_(obj));
}

CL_DOCSTRING("Return the index part of the stamp.  Stamp indices are adjacent to each other.");
CL_DEFUN size_t core__stamp_index(size_t stamp)
{
  return stamp>>gctools::Header_s::wtag_shift;
}

CL_DOCSTRING("Shift an unshifted stamp so that it can be put into code in a form where it can be directly matched to a stamp read from an object header with no further shifting");
CL_DEFUN core::Integer_sp core__shift_stamp_for_compiled_code(core::Integer_sp unshifted_stamp)
{
  core::Integer_sp shifted_stamp = core::cl__logior(core::Cons_O::createList(core::clasp_ash(unshifted_stamp,gctools::Header_s::stamp_shift),core::make_fixnum(gctools::Header_s::stamp_tag)));
  return shifted_stamp;
}

CL_DOCSTRING("Return the stamp for the object, the flags and the header stamp");
CL_DEFUN core::T_sp core__instance_stamp(core::T_sp obj)
{
  core::T_sp stamp((gctools::Tagged)cx_read_stamp(obj.raw_(),0));
  if (stamp.fixnump()) return stamp;
  SIMPLE_ERROR(BF("core:instance-stamp was about to return a non-fixnum %p") % (void*)stamp.raw_());
}

CL_DOCSTRING("Set the header stamp for the object");
CL_DEFUN void core__instance_stamp_set(core::T_sp obj, core::T_sp stamp)
{
  ASSERT(stamp.fixnump());
  if (gc::IsA<core::Instance_sp>(obj)) {
    core::Instance_sp iobj = gc::As_unsafe<core::Instance_sp>(obj);
    return iobj->stamp_set(stamp.unsafe_fixnum());
  } else if (gc::IsA<core::FuncallableInstance_sp>(obj)) {
    core::FuncallableInstance_sp iobj = gc::As_unsafe<core::FuncallableInstance_sp>(obj);
    return iobj->stamp_set(stamp.unsafe_fixnum());
  }
  SIMPLE_ERROR(BF("Only Instance and FuncallableInstance objects can have their stamp set") % _rep_(obj));
}


CL_LAMBDA(obj);
CL_DOCSTRING(R"doc(Return true if the object inherits from core:instance based on its header value)doc");
CL_DEFUN bool core__inherits_from_instance(core::T_sp obj)
{
  return (gc::IsA<core::Instance_sp>(obj));
}

};

#ifdef USE_BOEHM

struct ReachableClass {
  ReachableClass() : _Kind(gctools::STAMP_null){};
  ReachableClass(gctools::GCStampEnum tn) : _Kind(tn), instances(0), totalSize(0) {}
  void update(size_t sz) {
    ++this->instances;
    this->totalSize += sz;
  };
  gctools::GCStampEnum _Kind;
  size_t instances;
  size_t totalSize;
  size_t print(const std::string &shortName) {
    core::Fixnum k = this->_Kind;
    stringstream className;
    if (k <= gctools::STAMP_max) {
      const char* nm = obj_name(k);
      if (!nm) {
        className << "NULL-NAME";
      } else {
        className << nm;
      }
    } else {
      className << "??CONS??";
    }
      clasp_write_string((BF("%s: total_size: %10d count: %8d avg.sz: %8d kind: %s/%d\n")
                          % shortName % this->totalSize % this->instances
                          % (this->totalSize / this->instances) % className.str().c_str() % k).str()
                         ,cl::_sym_STARstandard_outputSTAR->symbolValue());
      core::clasp_finish_output_t();
      return this->totalSize;
  }
};


typedef map<gctools::GCStampEnum, ReachableClass> ReachableClassMap;
static ReachableClassMap *static_ReachableClassKinds;
static size_t invalidHeaderTotalSize = 0;
int globalSearchMarker = 0;
extern "C" {
void boehm_callback_reachable_object(void *ptr, size_t sz, void *client_data) {
  gctools::Header_s *h = reinterpret_cast<gctools::Header_s *>(ptr);
//  if (h->markerMatches(globalSearchMarker)) {
    ReachableClassMap::iterator it = static_ReachableClassKinds->find(h->stamp());
    if (it == static_ReachableClassKinds->end()) {
      ReachableClass reachableClass(h->stamp());
      reachableClass.update(sz);
      (*static_ReachableClassKinds)[h->stamp()] = reachableClass;
    } else {
      it->second.update(sz);
    }
//  } else {
//    invalidHeaderTotalSize += sz;
//  }
}
};

template <typename T>
size_t dumpResults(const std::string &name, const std::string &shortName, T *data) {
  typedef typename T::value_type::second_type value_type;
  vector<value_type> values;
  for (auto it : *data) {
    values.push_back(it.second);
  }
  size_t totalSize(0);
  sort(values.begin(), values.end(), [](const value_type &x, const value_type &y) {
            return (x.totalSize > y.totalSize);
  });
  size_t idx = 0;
  for (auto it : values) {
    // Does that print? If so should go to the OutputStream
    size_t sz = it.print(shortName);
    totalSize += sz;
    if (sz < 96) break;
    idx += 1;
#if 0
    if ( idx % 100 == 0 ) {
      gctools::poll_signals();
    }
#endif
  }
  return totalSize;
}

#endif

#ifdef USE_MPS
struct ReachableMPSObject {
  ReachableMPSObject(int k) : stamp(k) {};
  size_t stamp = 0;
  size_t instances = 0;
  size_t totalMemory = 0;
  size_t largest = 0;
  size_t print(const std::string &shortName,const vector<std::string> stampNames) {
    if (this->instances > 0) {
      clasp_write_string((BF("%s: total_size: %10d count: %8d avg.sz: %8d kind: %s/%d\n")
                          % shortName % this->totalMemory % this->instances % (this->totalMemory/this->instances)
                          % stampNames[this->stamp] % this->stamp).str(),
                         cl::_sym_STARstandard_outputSTAR->symbolValue());
      core::clasp_finish_output_t();
    }
    return this->totalMemory;
  }
};

extern "C" {
void amc_apply_stepper(mps_addr_t client, void *p, size_t s) {
  const gctools::Header_s *header = reinterpret_cast<const gctools::Header_s *>(gctools::ClientPtrToBasePtr(client));
  vector<ReachableMPSObject> *reachablesP = reinterpret_cast<vector<ReachableMPSObject> *>(p);
  if (header->stampP()) {
    ReachableMPSObject &obj = (*reachablesP)[header->stamp()];
    ++obj.instances;
    size_t sz = (char *)(obj_skip(client)) - (char *)client;
    obj.totalMemory += sz;
    if (sz > obj.largest)
      obj.largest = sz;
  }
}
};

size_t dumpMPSResults(const std::string &name, const std::string &shortName, vector<ReachableMPSObject> &values) {
  size_t totalSize(0);
  typedef ReachableMPSObject value_type;
  sort(values.begin(), values.end(), [](const value_type &x, const value_type &y) {
            return (x.totalMemory > y.totalMemory);
  });
  size_t idx = 0;
  vector<std::string> stampNames;
  stampNames.resize(gctools::global_NextStamp.load());
  for ( auto it : global_stamp_name_map ) {
    stampNames[it.second] = it.first;
  }
  for (auto it : values) {
    // Does that print? If so should go to the OutputStream
    totalSize += it.print(shortName,stampNames);
    idx += 1;
#if 0
    if ( idx % 100 == 0 ) {
      gctools::poll_signals();
    }
#endif
  }
  return totalSize;
}

#endif // USE_MPS

namespace gctools {

CL_LAMBDA(&optional x (marker 0));
CL_DEFUN core::T_mv gctools__gc_info(core::T_sp x, core::Fixnum_sp marker) {
#ifdef USE_MPS
  return Values(_Nil<core::T_O>());
#endif
#ifdef USE_BOEHM
  return Values(_Nil<core::T_O>());
#endif
};

CL_LAMBDA(on &key (backtrace-start 0) (backtrace-count 0) (backtrace-depth 6));
CL_DEFUN void gctools__monitor_allocations(bool on, core::Fixnum_sp backtraceStart, core::Fixnum_sp backtraceCount, core::Fixnum_sp backtraceDepth) {
#ifdef DEBUG_MONITOR_ALLOCATIONS
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
#endif
};

CL_LAMBDA(&optional marker);
CL_DEFUN Fixnum gctools__gc_marker(core::Fixnum_sp marker) {
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

CL_DEFUN void gctools__alloc_pattern_begin(core::Symbol_sp pattern) {
#ifdef USE_MPS
  mps_alloc_pattern_t mps_pat;
  if (pattern == _sym_ramp) {
    mps_pat = mps_alloc_pattern_ramp();
  } else if (pattern == _sym_rampCollectAll) {
    mps_pat = mps_alloc_pattern_ramp_collect_all();
  } else {
    TYPE_ERROR(pattern, core::Cons_O::createList(cl::_sym_or, _sym_ramp, _sym_rampCollectAll));
  }
  core::List_sp patternStack = gctools::_sym_STARallocPatternStackSTAR->symbolValue();
  patternStack = core::Cons_O::create(pattern, patternStack);
  gctools::_sym_STARallocPatternStackSTAR->setf_symbolValue(patternStack);
  mps_ap_alloc_pattern_begin(my_thread_allocation_points._automatic_mostly_copying_allocation_point, mps_pat);
  mps_ap_alloc_pattern_begin(my_thread_allocation_points._amc_cons_allocation_point, mps_pat);
  mps_ap_alloc_pattern_begin(my_thread_allocation_points._automatic_mostly_copying_zero_rank_allocation_point,mps_pat);
#endif
};

CL_DEFUN core::Symbol_sp gctools__alloc_pattern_end() {
  core::Symbol_sp pattern(_Nil<core::Symbol_O>());
#ifdef USE_MPS
  core::List_sp patternStack = gctools::_sym_STARallocPatternStackSTAR->symbolValue();
  if (patternStack.nilp())
    return _Nil<core::Symbol_O>();
  pattern = gc::As<core::Symbol_sp>(oCar(patternStack));
  gctools::_sym_STARallocPatternStackSTAR->setf_symbolValue(oCdr(patternStack));
  mps_alloc_pattern_t mps_pat;
  if (pattern == _sym_ramp) {
    mps_pat = mps_alloc_pattern_ramp();
  } else {
    mps_pat = mps_alloc_pattern_ramp_collect_all();
  }
  mps_ap_alloc_pattern_end(my_thread_allocation_points._automatic_mostly_copying_zero_rank_allocation_point,mps_pat);
  mps_ap_alloc_pattern_end(my_thread_allocation_points._amc_cons_allocation_point, mps_pat);
  mps_ap_alloc_pattern_end(my_thread_allocation_points._automatic_mostly_copying_allocation_point, mps_pat);
#endif
  return pattern;
};

CL_LAMBDA(&optional x (marker 0) msg);
CL_DECLARE();
CL_DOCSTRING("room - Return info about the reachable objects.  x can be T, nil, :default - as in ROOM.  marker can be a fixnum (0 - matches everything, any other number/only objects with that marker)");
CL_DEFUN core::T_mv cl__room(core::T_sp x, core::Fixnum_sp marker, core::T_sp tmsg) {
  std::ostringstream OutputStream;
  string smsg = "Total";
  if (cl__stringp(tmsg)) {
    core::String_sp msg = gc::As_unsafe<core::String_sp>(tmsg);
    smsg = msg->get();
  }
#ifdef USE_MPS
  mps_word_t numCollections = mps_collections(global_arena);
  size_t arena_committed = mps_arena_committed(global_arena);
  size_t arena_reserved = mps_arena_reserved(global_arena);
  vector<ReachableMPSObject> reachables;
  for (int i = 0; i < global_NextStamp.load(); ++i) {
    reachables.push_back(ReachableMPSObject(i));
  }
  mps_amc_apply(global_amc_pool, amc_apply_stepper, &reachables, 0);
  mps_amc_apply(global_amcz_pool, amc_apply_stepper, &reachables, 0);
  OutputStream << "-------------------- Reachable Kinds -------------------\n";
  dumpMPSResults("Reachable Kinds", "AMCpool", reachables);
  OutputStream << std::setw(12) << numCollections << " collections\n";
  OutputStream << std::setw(12) << arena_committed << " mps_arena_committed\n";
  OutputStream << std::setw(12) << arena_reserved << " mps_arena_reserved\n";
  OutputStream << std::setw(12) << globalMpsMetrics.finalizationRequests.load() << " finalization requests\n";
  size_t totalAllocations = globalMpsMetrics.nonMovingAllocations.load()
    + globalMpsMetrics.movingAllocations.load()
    + globalMpsMetrics.movingZeroRankAllocations.load()
    + globalMpsMetrics.unknownAllocations.load();
  OutputStream << std::setw(12) << totalAllocations << " total allocations\n";
  OutputStream << std::setw(12) <<  globalMpsMetrics.nonMovingAllocations.load() << "    non-moving(AWL) allocations\n";
  OutputStream << std::setw(12) << globalMpsMetrics.movingAllocations.load() << "    moving(AMC) allocations\n";
  OutputStream << std::setw(12) << globalMpsMetrics.movingZeroRankAllocations.load() << "    moving zero-rank(AMCZ) allocations\n";
  OutputStream << std::setw(12) << globalMpsMetrics.unknownAllocations.load() << "    unknown(configurable) allocations\n";
  OutputStream << std::setw(12) << globalMpsMetrics.totalMemoryAllocated.load() << " total memory allocated\n";
  OutputStream << std::setw(12) << global_NumberOfRootTables.load() << " module root tables\n";
  OutputStream << std::setw(12) << global_TotalRootTableSize.load() << " words - total module root table size\n";
                                                                
#endif
#ifdef USE_BOEHM
  globalSearchMarker = core::unbox_fixnum(marker);
  static_ReachableClassKinds = new (ReachableClassMap);
  invalidHeaderTotalSize = 0;
#ifdef BOEHM_GC_ENUMERATE_REACHABLE_OBJECTS_INNER_AVAILABLE
  GC_enumerate_reachable_objects_inner(boehm_callback_reachable_object, NULL);
  #else
  OutputStream <<  __FILE__ << ":" << __LINE__ <<  "The boehm function GC_enumerate_reachable_objects_inner is not available\n";
#endif
  OutputStream << "Walked LispKinds\n" ;
  size_t totalSize(0);
  OutputStream << "-------------------- Reachable ClassKinds -------------------\n"; 
  totalSize += dumpResults("Reachable ClassKinds", "class", static_ReachableClassKinds);
  OutputStream << "Skipping objects with less than 96 total_size\n";
  OutputStream << "Done walk of memory  " << static_cast<uintptr_t>(static_ReachableClassKinds->size()) << " ClassKinds\n";
#if USE_CXX_DYNAMIC_CAST
  OutputStream << smsg << " live memory total size = " << std::setw(12) << invalidHeaderTotalSize << '\n';
#else
  OutputStream << smsg << " invalidHeaderTotalSize = " << std::setw(12) << invalidHeaderTotalSize << '\n';
#endif
  OutputStream << smsg << " memory usage (bytes):    " << std::setw(12) << totalSize << '\n';
  OutputStream << smsg << " GC_get_heap_size()       " << std::setw(12) << GC_get_heap_size() << '\n';
  OutputStream << smsg << " GC_get_free_bytes()      " << std::setw(12) << GC_get_free_bytes() << '\n';
  OutputStream << smsg << " GC_get_bytes_since_gc()  " <<  std::setw(12) << GC_get_bytes_since_gc() << '\n';
  OutputStream << smsg << " GC_get_total_bytes()     " <<  std::setw(12) << GC_get_total_bytes() << '\n';

  delete static_ReachableClassKinds;
#endif

  clasp_write_string(OutputStream.str(),cl::_sym_STARstandard_outputSTAR->symbolValue());
  return Values(_Nil<core::T_O>());
};
};

namespace gctools {
#ifdef USE_MPS
CL_DEFUN void gctools__save_lisp_and_die(const std::string& filename)
{
  throw(core::SaveLispAndDie(filename));
}
CL_DEFUN void gctools__enable_underscanning(bool us)
{
  global_underscanning = us;
}

#endif
};

#ifdef DEBUG_FUNCTION_CALL_COUNTER
namespace gctools {
void common_function_call_counter(core::General_O* obj, size_t size, void* hash_table_raw) {
  core::HashTableEq_O* hash_table = reinterpret_cast<core::HashTableEq_O*>(hash_table_raw);
  core::T_sp gen = obj->asSmartPtr();
  if (core::Function_sp func = gen.asOrNull<core::Function_O>() ) {
    hash_table->setf_gethash(gen,core::clasp_make_fixnum(func->_TimesCalled));
  }
}


#ifdef USE_MPS
void amc_apply_function_call_counter(mps_addr_t client, void* hash_table_raw, size_t s)
{
  common_function_call_counter(reinterpret_cast<core::General_O*>(client),s,hash_table_raw);
}
#endif
#ifdef USE_BOEHM
void boehm_callback_function_call_counter(void* header, size_t size, void* hash_table_raw)
{
  common_function_call_counter(BasePtrToMostDerivedPtr<core::General_O>(header),
                               size, hash_table_raw);
};
#endif

CL_LAMBDA(func);
CL_DECLARE();
CL_DOCSTRING("function-call-count-profiler - Evaluate a function, count every function call made during the evaluation.");
CL_DEFUN void gctools__function_call_count_profiler(core::T_sp func) {
  core::HashTable_sp func_counters_start = core::HashTableEq_O::create_default();
  core::HashTable_sp func_counters_end = core::HashTableEq_O::create_default();
#ifdef USE_MPS
  mps_amc_apply(global_amc_pool, amc_apply_function_call_counter, &*func_counters_start, 0);
#endif
#ifdef USE_BOEHM
#ifdef BOEHM_GC_ENUMERATE_REACHABLE_OBJECTS_INNER_AVAILABLE
  GC_enumerate_reachable_objects_inner(boehm_callback_function_call_counter, &*func_counters_start);
#endif
#endif
  core::eval::funcall(func);
#ifdef USE_MPS
  mps_amc_apply(global_amc_pool, amc_apply_function_call_counter, &*func_counters_end, 0);
#endif
#ifdef USE_BOEHM
#ifdef BOEHM_GC_ENUMERATE_REACHABLE_OBJECTS_INNER_AVAILABLE
  GC_enumerate_reachable_objects_inner(boehm_callback_function_call_counter, &*func_counters_end);
#endif
#endif
  func_counters_start->mapHash([func_counters_end](core::T_sp f, core::T_sp start_value) {
      core::T_sp end_value = func_counters_end->gethash(f);
      ASSERT(start_value.fixnump() && end_value.fixnump());
      Fixnum diff = end_value.unsafe_fixnum() - start_value.unsafe_fixnum();
      func_counters_end->setf_gethash(f,core::clasp_make_fixnum(diff));
    } );

  core::List_sp results = _Nil<core::T_O>();
  func_counters_end->mapHash([func_counters_end,&results](core::T_sp f, core::T_sp value) {
      ASSERT(value.fixnump());
      Fixnum diff = value.unsafe_fixnum();
      if ( diff > 0 ) {
        results = core::Cons_O::create(core::Cons_O::create(core::clasp_make_fixnum(diff),f),results);
      }
    });
  printf("%s:%d There are %zu results\n", __FILE__, __LINE__, core::cl__length(results));
  results = core::cl__sort(results,cl::_sym__LT_,cl::_sym_car);
  for ( auto cur : results ) {
    core::T_sp one = oCar(cur);
    core::T_sp count = oCar(one);
    core::T_sp func = oCdr(one);
    if ( count.unsafe_fixnum() > 0) {
      core::write_bf_stream(BF("%d : %s\n") % count.unsafe_fixnum() % _rep_(func));
    }
  }
};
};
#endif // DEBUG_FUNCTION_CALL_COUNTER

namespace gctools {
SYMBOL_EXPORT_SC_(GcToolsPkg,STARfinalizersSTAR);
/*! Call finalizer_callback with no arguments when object is finalized.*/
CL_DEFUN void gctools__finalize(core::T_sp object, core::T_sp finalizer_callback) {
  //printf("%s:%d making a finalizer for %p calling %p\n", __FILE__, __LINE__, (void*)object.tagged_(), (void*)finalizer_callback.tagged_());
  core::WeakKeyHashTable_sp ht = As<core::WeakKeyHashTable_sp>(_sym_STARfinalizersSTAR->symbolValue());
  core::List_sp orig_finalizers = ht->gethash(object,_Nil<core::T_O>());
  core::List_sp finalizers = core::Cons_O::create(finalizer_callback,orig_finalizers);
//  printf("%s:%d      Adding finalizer to list new length --> %d   list head %p\n", __FILE__, __LINE__, core::cl__length(finalizers), (void*)finalizers.tagged_());
  ht->hash_table_setf_gethash(object,finalizers);
    // Register the finalizer with the GC
#ifdef USE_BOEHM
  boehm_set_finalizer_list(object.tagged_(),finalizers.tagged_());
#endif
#ifdef USE_MPS
  if (object.generalp() || object.consp()) {
    my_mps_finalize((void*)&*object);
  }
#endif
};

CL_DEFUN void gctools__definalize(core::T_sp object) {
//  printf("%s:%d erasing finalizers for %p\n", __FILE__, __LINE__, (void*)object.tagged_());
  core::WeakKeyHashTable_sp ht = As<core::WeakKeyHashTable_sp>(_sym_STARfinalizersSTAR->symbolValue());
  if (ht->gethash(object)) {
    ht->remhash(object);
  }
#ifdef USE_BOEHM
  boehm_clear_finalizer_list(object.tagged_());
#endif
  #ifdef USE_MPS
  // Don't use mps_definalize here - definalize is taken care of by erasing the weak-key-hash-table
  // entry above.  We may still need to get a finalization message if this class needs
  // its destructor be called.
  #endif
}

};



extern "C" {
void dbg_room() {
  cl__room(_Nil<core::T_O>(), core::make_fixnum(0), _Nil<core::T_O>());
}
}
namespace gctools {

#ifdef DEBUG_COUNT_ALLOCATIONS
CL_DOCSTRING("Start collecting backtraces for allocations of a particular stamp. Write the bactraces into the file specified by filename.");
CL_DEFUN void gctools__start_collecting_backtraces_for_allocations_by_stamp(const std::string& filename, Fixnum stamp) {
  gctools::start_backtrace_allocations(filename,stamp);
}

CL_DOCSTRING("Stop collecting backtraces for allocations of a particular stamp.");
CL_DEFUN void gctools__stop_collecting_backtraces_for_allocations_by_stamp() {
  gctools::stop_backtrace_allocations();
}
#endif


CL_DEFUN void gctools__telemetryFlush() {
#ifdef USE_BOEHM
  IMPLEMENT_ME();
#endif
#ifdef USE_MPS
  mps_telemetry_flush();
#endif
};

CL_DEFUN void gctools__telemetrySet(core::Fixnum_sp flags) {
#ifdef USE_BOEHM
  IMPLEMENT_ME();
#endif
#ifdef USE_MPS
  mps_telemetry_set(unbox_fixnum(flags));
#endif
};

CL_DEFUN void gctools__telemetryReset(core::Fixnum_sp flags) {
#ifdef USE_BOEHM
  IMPLEMENT_ME();
#endif
#ifdef USE_MPS
  mps_telemetry_reset(unbox_fixnum(flags));
#endif
};

CL_DEFUN core::T_mv gctools__memory_profile_status() {
  int64_t allocationNumberCounter = my_thread_low_level->_Allocations._AllocationNumberCounter;
  size_t allocationNumberThreshold = my_thread_low_level->_Allocations._AllocationNumberThreshold;
  int64_t allocationSizeCounter = my_thread_low_level->_Allocations._AllocationSizeCounter;
  size_t allocationSizeThreshold = my_thread_low_level->_Allocations._AllocationSizeThreshold;
  return Values(core::make_fixnum(allocationNumberCounter),
                core::make_fixnum(allocationNumberThreshold),
                core::make_fixnum(allocationSizeCounter),
                core::make_fixnum(allocationSizeThreshold));
}

                
CL_DEFUN core::T_sp gctools__stack_depth() {
  int z = 0;
  void *zp = &z;
  size_t stackDepth = (char *)_global_stack_marker - (char *)zp;
  return core::make_fixnum((uint)stackDepth);
};

CL_DEFUN void gctools__garbage_collect() {
#ifdef USE_BOEHM
  GC_gcollect();
//  write_bf_stream(BF("GC_invoke_finalizers\n"));
  GC_invoke_finalizers();
#endif
//        printf("%s:%d Starting garbage collection of arena\n", __FILE__, __LINE__ );
#ifdef USE_MPS
  mps_arena_collect(global_arena);
  size_t finalizations;
  processMpsMessages(finalizations);
  mps_arena_release(global_arena);
#endif
  //        printf("Garbage collection done\n");
};

CL_DEFUN void gctools__register_stamp_name(const std::string& name,size_t stamp_num)
{
  register_stamp_name(name,stamp_num);
}

CL_DEFUN core::T_sp gctools__get_stamp_name_map() {
  DEPRECATED();
  core::List_sp l = _Nil<core::T_O>();
  for ( auto it : global_unshifted_nowhere_stamp_name_map ) {
    l = core::Cons_O::create(core::Cons_O::create(core::SimpleBaseString_O::make(it.first),core::make_fixnum(it.second)),l);
  }
  return l;
}

CL_DOCSTRING("Process finalizers");
CL_LAMBDA(&optional verbose)
CL_DEFUN void gctools__cleanup(bool verbose) {
#ifdef USE_MPS
  size_t finalizations;
  size_t messages = processMpsMessages(finalizations);
  if (verbose) {
    core::write_bf_stream(BF("Processed %d finalization messages and %d total messages\n") % messages % finalizations );
  }
#endif
}

CL_DOCSTRING("Set the number of signal polling ticks per GC cleanup and message processing.");
CL_LAMBDA(&optional verbose);
CL_DEFUN void gctools__poll_ticks_per_cleanup(int ticks) {
#ifdef USE_MPS
  global_pollTicksPerCleanup = ticks;
#endif
};



#define ARGS_gctools__debug_allocations "(arg)"
#define DECL_gctools__debug_allocations ""
#define DOCS_gctools__debug_allocations "debugAllocations"
CL_DEFUN void gctools__debug_allocations(core::T_sp debugOn) {
  _GlobalDebugAllocations = debugOn.isTrue();
};

bool debugging_configuration(bool setFeatures, bool buildReport, stringstream& ss) {
  bool metrics_file = false;
  core::List_sp features = cl::_sym_STARfeaturesSTAR->symbolValue();
  bool debugging = false;
  bool use_boehm_memory_marker = false;
#ifdef USE_BOEHM_MEMORY_MARKER
  use_boehm_memory_marker = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("USE_BOEHM_MEMORY_MARKER = %s\n") % (use_boehm_memory_marker ? "**DEFINED**" : "undefined") ).str();

  bool mps_recognize_all_tags = false;
#ifdef MPS_RECOGNIZE_ALL_TAGS
  mps_recognize_all_tags = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("MPS_RECOGNIZE_ALL_TAGS = %s\n") % (mps_recognize_all_tags ? "**DEFINED**" : "undefined") ).str();

  bool mps_recognize_zero_tags = false;
#ifdef MPS_RECOGNIZE_ZERO_TAGS
  mps_recognize_zero_tags = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("MPS_RECOGNIZE_ZERO_TAGS = %s\n") % (mps_recognize_zero_tags ? "**DEFINED**" : "undefined") ).str();

  bool use_symbols_in_global_array = false;
#ifdef USE_SYMBOLS_IN_GLOBAL_ARRAY
  use_symbols_in_global_array = true;
#endif
  if (buildReport) ss << (BF("USE_SYMBOLS_IN_GLOBAL_ARRAY = %s\n") % (use_symbols_in_global_array ? "**DEFINED**" : "undefined") ).str();

  bool use_static_analyzer_global_symbols = false;
#ifdef USE_STATIC_ANALYZER_GLOBAL_SYMBOLS
  use_static_analyzer_global_symbols = true;
#endif
  if (buildReport) ss << (BF("USE_STATIC_ANALYZER_GLOBAL_SYMBOLS = %s\n") % (use_static_analyzer_global_symbols ? "**DEFINED**" : "undefined") ).str();

  
  bool debug_throw_if_invalid_client_on = false;
#ifdef DEBUG_THROW_IF_INVALID_CLIENT_ON
  debug_throw_if_invalid_client_on = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("DEBUG_THROW_IF_INVALID_CLIENT_ON = %s\n") % (debug_throw_if_invalid_client_on ? "**DEFINED**" : "undefined") ).str();

    bool debug_telemetry = false;
#ifdef DEBUG_TELEMETRY
  debug_telemetry = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("DEBUG_TELEMETRY = %s\n") % (debug_telemetry ? "**DEFINED**" : "undefined") ).str();

  bool debug_stackmaps = false;
#ifdef DEBUG_STACKMAPS
  debug_stackmaps = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("DEBUG_STACKMAPS = %s\n") % (debug_stackmaps ? "**DEFINED**" : "undefined") ).str();

  bool debug_stack_telemetry = false;
#ifdef DEBUG_STACK_TELEMETRY
  debug_stack_telemetry = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("DEBUG_STACK_TELEMETRY = %s\n") % (debug_stack_telemetry ? "**DEFINED**" : "undefined") ).str();

  bool debug_mps_underscanning = false;
#ifdef DEBUG_MPS_UNDERSCANNING
  debug_mps_underscanning = true;
  bool debug_mps_underscanning_initial = DEBUG_MPS_UNDERSCANNING_INITIAL;
  debugging = true;
#else
  bool debug_mps_underscanning_initial = false;
#endif
  if (buildReport) ss << (BF("DEBUG_MPS_UNDERSCANNING = %s\n") % (debug_mps_underscanning ? "**DEFINED**" : "undefined") ).str();
  if (buildReport) ss << (BF("DEBUG_MPS_UNDERSCANNING_INITIAL = %s\n") % (debug_mps_underscanning_initial ? "true" : "false") ).str();

  bool debug_recursive_allocations = false;
#ifdef DEBUG_RECURSIVE_ALLOCATIONS
  debug_recursive_allocations = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("DEBUG_RECURSIVE_ALLOCATIONS = %s\n") % (debug_recursive_allocations ? "**DEFINED**" : "undefined") ).str();

  bool config_var_cool = false;
#ifdef CONFIG_VAR_COOL
  config_var_cool = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("CONFIG_VAR_COOL = %s\n") % (config_var_cool ? "**DEFINED**" : "undefined") ).str();

  bool debug_guard = false;
#ifdef DEBUG_GUARD
  debug_guard = true;
  debugging = true;
  if (setFeatures)  features = core::Cons_O::create(_lisp->internKeyword("DEBUG-GUARD"), features);
#endif
  if (buildReport) ss << (BF("DEBUG_GUARD = %s\n") % (debug_guard ? "**DEFINED**" : "undefined") ).str();

  bool debug_validate_guard = false;
#ifdef DEBUG_VALIDATE_GUARD
  debug_validate_guard = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("DEBUG_VALIDATE_GUARD = %s\n") % (debug_validate_guard ? "**DEFINED**" : "undefined") ).str();

  bool debug_function_call_counter = false;
#ifdef DEBUG_FUNCTION_CALL_COUNTER
  debug_function_call_counter = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("DEBUG_FUNCTION_CALL_COUNTER = %s\n") % (debug_function_call_counter ? "**DEFINED**" : "undefined") ).str();

  bool debug_ensure_valid_object = false;
#ifdef DEBUG_ENSURE_VALID_OBJECT
  debug_ensure_valid_object = true;
  debugging = true;
  if (setFeatures)  features = core::Cons_O::create(_lisp->internKeyword("DEBUG-ENSURE-VALID-OBJECT"),features);
#endif  
  if (buildReport) ss << (BF("DEBUG_ENSURE_VALID_OBJECT = %s\n") % (debug_ensure_valid_object ? "**DEFINED**" : "undefined") ).str();
  
  bool debug_cache = false;
#ifdef DEBUG_CACHE
  debug_cache = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("DEBUG_CACHE = %s\n") % (debug_cache ? "**DEFINED**" : "undefined") ).str();

  bool debug_threads = false;
#ifdef DEBUG_THREADS
  debug_threads = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("DEBUG_THREADS = %s\n") % (debug_threads ? "**DEFINED**" : "undefined") ).str();

  bool debug_gfdispatch = false;
#ifdef DEBUG_GFDISPATCH
  debug_gfdispatch = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("DEBUG_GFDISPATCH = %s\n") % (debug_gfdispatch ? "**DEFINED**" : "undefined") ).str();

  bool debug_cst = false;
#ifdef CST
  debug_cst = true;
  debugging = true;
  if (setFeatures)  features = core::Cons_O::create(_lisp->internKeyword("CST"),features);
#endif
  if (buildReport) ss << (BF("CST = %s\n") % (debug_gfdispatch ? "**DEFINED**" : "undefined") ).str();

  bool debug_ihs = false;
#ifdef DEBUG_IHS
  debug_ihs = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("DEBUG_IHS = %s\n") % (debug_ihs ? "**DEFINED**" : "undefined") ).str();

  bool debug_enable_profiling = false;
#ifdef ENABLE_PROFILING
  debug_enable_profiling = true;
  debugging = true;
  if (setFeatures)  features = core::Cons_O::create(_lisp->internKeyword("ENABLE-PROFILING"),features);
#endif
  if (buildReport) ss << (BF("ENABLE_PROFILING = %s\n") % (debug_enable_profiling ? "**DEFINED**" : "undefined") ).str();

  bool debug_release = false;
#ifdef DEBUG_RELEASE
  debug_release = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("DEBUG_RELEASE = %s\n") % (debug_release ? "**DEFINED**" : "undefined") ).str();

  bool debug_bounds_assert = false;
#ifdef DEBUG_BOUNDS_ASSERT
  debug_bounds_assert = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("DEBUG_BOUNDS_ASSERT = %s\n") % (debug_bounds_assert ? "**DEFINED**" : "undefined") ).str();

  bool debug_slot_accessors = false;
#ifdef DEBUG_SLOT_ACCESSORS
  debug_slot_accessors = true;
  debugging = true;
  if (setFeatures)  features = core::Cons_O::create(_lisp->internKeyword("DEBUG-SLOT-ACCESSORS"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_SLOT_ACCESSORS = %s\n") % (debug_slot_accessors ? "**DEFINED**" : "undefined") ).str();

  bool debug_fastgf = false;
#ifdef DEBUG_FASTGF
  debug_fastgf = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-FASTGF"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_FASTGF = %s\n") % (debug_fastgf ? "**DEFINED**" : "undefined") ).str();
 
 bool debug_rehash_count = false;
#ifdef DEBUG_REHASH_COUNT
 #ifndef DEBUG_MONITOR
   #error "DEBUG_MONITOR must also be enabled if DEBUG_REHASH_COUNT is turned on"
 #endif
  debug_rehash_count = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-REHASH_COUNT"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_REHASH_COUNT = %s\n") % (debug_rehash_count ? "**DEFINED**" : "undefined") ).str();

    
  bool debug_jit_log_symbols = false;
#ifdef DEBUG_JIT_LOG_SYMBOLS
  if (!core::global_options->_SilentStartup) {
    printf("%s:%d  Setting JIT-LOG-SYMBOLS *feature*\n", __FILE__, __LINE__ );
  }
  debug_jit_log_symbols = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("JIT-LOG-SYMBOLS"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_JIT_LOG_SYMBOLS = %s\n") % (debug_jit_log_symbols ? "**DEFINED**" : "undefined") ).str();


  bool debug_mps_size = false;
#ifdef DEBUG_MPS_SIZE
  debug_mps_size = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-MPS_SIZE"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_MPS_SIZE = %s\n") % (debug_mps_size ? "**DEFINED**" : "undefined") ).str();


  bool debug_bclasp_lisp = false;
#ifdef DEBUG_BCLASP_LISP
  debug_bclasp_lisp = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-BCLASP-LISP"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_BCLASP_LISP = %s\n") % (debug_bclasp_lisp ? "**DEFINED**" : "undefined") ).str();
  
  bool debug_flow_tracker = false;
#ifdef DEBUG_FLOW_TRACKER
  debug_flow_tracker = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-FLOW-TRACKER"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_FLOW_TRACKER = %s\n") % (debug_flow_tracker ? "**DEFINED**" : "undefined") ).str();

    bool debug_track_unwinds = false;
#ifdef DEBUG_TRACK_UNWINDS
  debug_track_unwinds = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-TRACK-UNWINDS"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_TRACK_UNWINDS = %s\n") % (debug_track_unwinds ? "**DEFINED**" : "undefined") ).str();

  bool track_allocations = false;
#ifdef DEBUG_TRACK_ALLOCATIONS
  track_allocations = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("TRACK-ALLOCATIONS"),features);
#endif
  if (buildReport) ss << (BF("TRACK_ALLOCATIONS = %s\n") % (track_allocations ? "**DEFINED**" : "undefined") ).str();

    bool debug_lexical_depth = false;
#ifdef DEBUG_LEXICAL_DEPTH
  debug_lexical_depth = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-LEXICAL-DEPTH"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_LEXICAL_DEPTH = %s\n") % (debug_lexical_depth ? "**DEFINED**" : "undefined") ).str();

  bool debug_dtree_interpreter = false;
#ifdef DEBUG_DTREE_INTERPRETER
  debug_dtree_interpreter = true;
#ifndef DEBUG_MONITOR_SUPPORT
#error "You must enable DEBUG_MONITOR_SUPPORT to use DEBUG_DTREE_INTERPRETER"
#endif
  debugging = true;
#endif
  if (buildReport) ss << (BF("DEBUG_DTREE_INTERPRETER = %s\n") % (debug_dtree_interpreter ? "**DEFINED**" : "undefined") ).str();
  
  bool debug_cclasp_lisp = false;
#ifdef DEBUG_CCLASP_LISP
  debug_cclasp_lisp = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-CCLASP-LISP"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_CCLASP_LISP = %s\n") % (debug_cclasp_lisp ? "**DEFINED**" : "undefined") ).str();

    bool debug_long_call_history = false;
#ifdef DEBUG_LONG_CALL_HISTORY
  debug_long_call_history = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-LONG-CALL-HISTORY"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_LONG_CALL_HISTORY = %s\n") % (debug_long_call_history ? "**DEFINED**" : "undefined") ).str();

  bool debug_memory_profile = false;
#ifdef DEBUG_MEMORY_PROFILE
  debug_memory_profile = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-MEMORY-PROFILE"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_MEMORY_PROFILE = %s\n") % (debug_memory_profile ? "**DEFINED**" : "undefined") ).str();

  bool debug_compiler = false;
#ifdef DEBUG_COMPILER
  debug_compiler = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-COMPILER"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_COMPILER = %s\n") % (debug_compiler ? "**DEFINED**" : "undefined") ).str();

  bool debug_assert_type_cast = false;
#ifdef DEBUG_ASSERT_TYPE_CAST
  debug_assert_type_cast = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-ASSERT-TYPE-CAST"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_ASSERT_TYPE_CAST = %s\n") % (debug_assert_type_cast ? "**DEFINED**" : "undefined") ).str();

  bool debug_llvm_optimization_level_0 = false;
#ifdef DEBUG_LLVM_OPTIMIZATION_LEVEL_0
  debug_llvm_optimization_level_0 = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-LLVM-OPTIMIZATION-LEVEL-0"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_LLVM_OPTIMIZATION_LEVEL_0 = %s\n") % (debug_llvm_optimization_level_0 ? "**DEFINED**" : "undefined") ).str();

  bool debug_count_allocations = false;
#ifdef DEBUG_COUNT_ALLOCATIONS
  debug_count_allocations = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-COUNT-ALLOCATIONS"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_COUNT_ALLOCATIONS = %s\n") % (debug_count_allocations ? "**DEFINED**" : "undefined") ).str();

  bool debug_dont_optimize_bclasp = false;
#ifdef DEBUG_DONT_OPTIMIZE_BCLASP
  debug_dont_optimize_bclasp = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-DONT-OPTIMIZE-BCLASP"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_DONT_OPTIMIZE_BCLASP = %s\n") % (debug_dont_optimize_bclasp ? "**DEFINED**" : "undefined") ).str();

  bool debug_dtrace_lock_probe = false;
#ifdef DEBUG_DTRACE_LOCK_PROBE
  debug_dtrace_lock_probe = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-DTRACE-LOCK-PROBE"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_DTRACE_LOCK_PROBE = %s\n") % (debug_dtrace_lock_probe ? "**DEFINED**" : "undefined") ).str();

  bool disable_type_inference = false;
#ifdef DISABLE_TYPE_INFERENCE
  disable_type_inference = true;
  debugging = true;
  if (setFeatures)  features = core::Cons_O::create(_lisp->internKeyword("DISABLE-TYPE-INFERENCE"),features);
#endif
  if (buildReport) ss << (BF("DISABLE_TYPE_INFERENCE = %s\n") % (disable_type_inference ? "**DEFINED**" : "undefined") ).str();

  bool use_lto = false;
  // CLASP_BUILD_MODE == 0 means generate fasls
#if CLASP_BUILD_MODE == 0
  use_lto = false;
  debugging = true;
  INTERN_(core,STARclasp_build_modeSTAR)->defparameter(kw::_sym_fasl);
  // CLASP_BUILD_MODE == 1 means generate object files
#elif CLASP_BUILD_MODE == 1
  use_lto = false;
  debugging = true;
  INTERN_(core,STARclasp_build_modeSTAR)->defparameter(kw::_sym_object);
  // CLASP_BUILD_MODE == 2 means generate bitcode and use thinlto
#elif CLASP_BUILD_MODE == 2
  use_lto = true;
  debugging = false;
  INTERN_(core,STARclasp_build_modeSTAR)->defparameter(kw::_sym_bitcode);
#endif
  if (buildReport) ss << (BF("CLASP_BUILD_MODE = %s") % CLASP_BUILD_MODE);
  
  bool use_human_readable_bitcode = false;
#if USE_HUMAN_READABLE_BITCODE==1
  use_human_readable_bitcode = true;
  debugging = true;
  if (setFeatures)  features = core::Cons_O::create(_lisp->internKeyword("USE-HUMAN-READABLE-BITCODE"),features);
#endif
  if (buildReport) ss << (BF("USE_HUMAN_READABLE_BITCODE = %s\n") % (use_human_readable_bitcode ? "**DEFINED**" : "undefined") ).str();

  //
  // DEBUG_MONITOR must be last - other options turn this on
  //
  bool debug_monitor = false;
#ifdef DEBUG_MONITOR
#ifndef DEBUG_MONITOR_SUPPORT
#error "You must enable DEBUG_MONITOR_SUPPORT to use DEBUG_DTREE_INTERPRETER"
#endif
  debug_monitor = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-MONITOR"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_MONITOR = %s\n") % (debug_monitor ? "**DEFINED**" : "undefined") ).str();

  bool debug_monitor_support = false;
#ifdef DEBUG_MONITOR_SUPPORT
  debug_monitor_support = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-MONITOR-SUPPORT"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_MONITOR_SUPPORT = %s\n") % (debug_monitor_support ? "**DEFINED**" : "undefined") ).str();
  
  // -------------------------------------------------------------
  //
  // The cl:*features* environment variable is set below - so all changes to (features) must be above
  //
  if (setFeatures) cl::_sym_STARfeaturesSTAR->setf_symbolValue(features);

  // ---- return with the debugging flag
  return debugging;
}

CL_DEFUN void gctools__configuration()
{
  stringstream ss;
  bool debugging = debugging_configuration(false,true,ss);
  core::clasp_writeln_string(ss.str());
}


void initialize_gc_functions() {
  _sym_STARallocPatternStackSTAR->defparameter(_Nil<core::T_O>());
#ifdef USE_MPS
//  core::af_def(GcToolsPkg, "mpsTelemetrySet", &gctools__mpsTelemetrySet);
//  core::af_def(GcToolsPkg, "mpsTelemetryReset", &gctools__mpsTelemetryReset);
//  core::af_def(GcToolsPkg, "mpsTelemetryFlush", &gctools__mpsTelemetryFlush);
#endif
};
};
