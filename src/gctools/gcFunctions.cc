/* -^- */
int gcFunctions_top;
#include <clasp/core/foundation.h>

#include <boost/mpl/list.hpp>
#ifdef USE_BOEHM
#include "src/bdwgc/include/gc_mark.h"
#endif
int gcFunctions_before;
#ifdef USE_MPS
extern "C" {
#include <clasp/mps/code/mpscamc.h>
};
#endif

int gcFunctions_after;

#include <stdint.h>
#include <unistd.h>
#include <sstream>
#include <iomanip>

#include <clasp/core/object.h>
#include <clasp/core/bformat.h>
#include <clasp/core/lisp.h>
#include <clasp/core/instance.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/pathname.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/llvmo/code.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/debugInfoExpose.h>
#include <clasp/gctools/gc_interface.h>
#include <clasp/gctools/threadlocal.h>
#include <clasp/gctools/snapshotSaveLoad.h>
#include <clasp/core/compiler.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/wrappers.h>

namespace gctools {
std::atomic<double> global_DiscriminatingFunctionCompilationSeconds(0.0);

DOCGROUP(clasp);
CL_DEFUN void gctools__accumulate_discriminating_function_compilation_seconds(double seconds) {
  global_DiscriminatingFunctionCompilationSeconds = global_DiscriminatingFunctionCompilationSeconds + seconds;
}

DOCGROUP(clasp);
CL_DEFUN double gctools__discriminating_function_compilation_seconds() { return global_DiscriminatingFunctionCompilationSeconds; }

}; // namespace gctools

namespace gctools {

size_t global_next_unused_kind = STAMPWTAG_max + 1;

/*! Hardcode a few kinds of objects for bootstrapping
 */

const char* global_HardcodedKinds[] = {"", "core::T_O", "core::Instance_O", "core::Symbol_O"};

DOCGROUP(clasp);
CL_DEFUN int gctools__max_bootstrap_kinds() { return sizeof(global_HardcodedKinds) / sizeof(global_HardcodedKinds[0]); }

int iBootstrapKind(const string& name) {
  for (int i(0), iEnd(gctools__max_bootstrap_kinds()); i < iEnd; ++i) {
    //            printf("%s:%d i[%d]Checking if %s == %s\n", __FILE__, __LINE__, i, global_HardcodedKinds[i], name.c_str());
    if (strcmp(global_HardcodedKinds[i], name.c_str()) == 0) {
      return i;
    }
  }
  SIMPLE_ERROR("Illegal bootstrap-kind {}", name);
}

DOCGROUP(clasp);
CL_DEFUN core::Cons_sp gctools__bootstrap_kind_symbols() {
  core::Cons_sp list(nil<core::Cons_O>());
  for (int i(gctools__max_bootstrap_kinds() - 1); i > 0; --i) {
    string name = global_HardcodedKinds[i];
    list = core::Cons_O::create(core::SimpleBaseString_O::make(name), list);
  }
  return list;
}

DOCGROUP(clasp);
CL_DEFUN core::T_sp gctools__bootstrap_kind_p(const string& name) {
  for (int i(0), iEnd(gctools__max_bootstrap_kinds()); i < iEnd; ++i) {
    //            printf("%s:%d i[%d]Checking if %s == %s\n", __FILE__, __LINE__, i, global_HardcodedKinds[i], name.c_str());
    if (strcmp(global_HardcodedKinds[i], name.c_str()) == 0) {
      return core::make_fixnum(i);
    }
  }
  return nil<core::T_O>();
}

DOCGROUP(clasp);
CL_DEFUN size_t gctools__thread_local_unwind_counter() { return my_thread->_unwinds; }

DOCGROUP(clasp);
CL_DEFUN void gctools__change_sigchld_sigport_handlers() {
  void* old = (void*)signal(SIGCHLD, SIG_DFL);
  signal(SIGPIPE, SIG_DFL);
  printf("%s:%d old signal handler for SIGCHLD = %p   SIG_DFL = %p\n", __FILE__, __LINE__, old, SIG_DFL);
}

DOCGROUP(clasp);
CL_DEFUN void gctools__deallocate_unmanaged_instance(core::T_sp obj) { obj_deallocate_unmanaged_instance(obj); }

CL_DOCSTRING(R"dx(Return bytes allocated (values clasp-calculated-bytes))dx");
DOCGROUP(clasp);
CL_DEFUN core::T_sp gctools__bytes_allocated() {
  size_t my_bytes = my_thread_low_level->_Allocations._BytesAllocated;
  return core::Integer_O::create(my_bytes);
}

CL_DOCSTRING(R"dx(Return the next unused kind)dx");
DOCGROUP(clasp);
CL_DEFUN size_t core__next_unused_kind() {
  size_t next = global_next_unused_kind;
  ++global_next_unused_kind;
  return next;
}

}; // namespace gctools

namespace gctools {

CL_DOCSTRING(R"dx(Return the header stamp for the object)dx");
DOCGROUP(clasp);
CL_DEFUN core::T_sp core__header_stamp(core::T_sp obj) {
  if (obj.generalp()) {
    void* mostDerived = gctools::untag_general<void*>(obj.raw_());
    const gctools::Header_s* header = reinterpret_cast<const gctools::Header_s*>(gctools::GeneralPtrToHeaderPtr(mostDerived));
    uintptr_t stamp = 0xFFFFFFFF & (header->_badge_stamp_wtag_mtag._value);
    //    core::clasp_write_string(fmt::format("{}:{}:{} stamp = {}u\n", __FILE__, __LINE__, __FUNCTION__, stamp ));
    core::T_sp result((gctools::Tagged)stamp);
    return result;
    ;
  }
  SIMPLE_ERROR("The object {} is not a general object and doesn't have a header-value", _rep_(obj));
}

CL_DOCSTRING(R"dx(Return the header value for the object)dx");
DOCGROUP(clasp);
CL_DEFUN core::T_sp core__header_value(core::T_sp obj) {
  if (obj.generalp()) {
    void* mostDerived = gctools::untag_general<void*>(obj.raw_());
    const gctools::Header_s* header = reinterpret_cast<const gctools::Header_s*>(gctools::GeneralPtrToHeaderPtr(mostDerived));
    return core::clasp_make_integer(header->_badge_stamp_wtag_mtag._value);
  }
  SIMPLE_ERROR("The object {} is not a general object and doesn't have a header-value", _rep_(obj));
}

CL_DOCSTRING(R"dx(Return the index part of the stamp.  Stamp indices are adjacent to each other.)dx");
DOCGROUP(clasp);
CL_DEFUN size_t core__stamp_index(size_t stamp) {
  return stamp >> (gctools::Header_s::wtag_width + gctools::Header_s::general_mtag_width);
}

CL_DOCSTRING(
    R"dx(Shift an unshifted stamp so that it can be put into code in a form where it can be directly matched to a stamp read from an object header with no further shifting)dx");
DOCGROUP(clasp);
CL_DEFUN core::Integer_sp core__shift_stamp_for_compiled_code(size_t stamp_wtagx2) {
  // This assumes the stamp_wtagx2 is a stamp_wtag shifted * 2
  // This shift must coordinate with the shift in GenerateTypeqHeaderValue
  // so that the returned result, when written into llvm-IR matches exactly the bit patterns
  // in the header._value
  //  If a stamp_wtag is 4
  //     then stamp_wtagx2 is 8
  //     This returns 32
  //     The bit pattern in a header._value will be 32
  return core::make_fixnum(stamp_wtagx2 << fixnum_shift);
}

CL_DOCSTRING(R"dx(Return the stamp for the object, the flags and the header stamp)dx");
DOCGROUP(clasp);
CL_DEFUN core::T_sp core__instance_stamp(core::T_sp obj) {
  core::T_sp stamp((gctools::Tagged)cx_read_stamp(obj.raw_(), 0));
  if (stamp.fixnump())
    return stamp;
  SIMPLE_ERROR("core:instance-stamp was about to return a non-fixnum {}", (void*)stamp.raw_());
}

CL_DOCSTRING(R"dx(Return the tagged pointer for the object, the flags and the header stamp)dx");
DOCGROUP(clasp);
CL_DEFUN core::T_sp core__instance_tagged_pointer(core::T_sp obj) { return core::Pointer_O::create(obj.raw_()); }

CL_DOCSTRING(R"dx(Determine if stamp A is immediately less than stamp B, so that they can be merged into a range.)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__stamps_adjacent_p(size_t stamp_a, size_t stamp_b) {
  return (((stamp_a >> gctools::Header_s::general_mtag_shift) + 1) == (stamp_b >> gctools::Header_s::general_mtag_shift));
}

CL_DOCSTRING(R"dx(Set the header stamp for the object)dx");
DOCGROUP(clasp);
CL_DEFUN void core__instance_stamp_set(core::T_sp obj, core::T_sp stamp) {
  ASSERT(stamp.fixnump());
  if (gc::IsA<core::Instance_sp>(obj)) {
    core::Instance_sp iobj = gc::As_unsafe<core::Instance_sp>(obj);
    return iobj->stamp_set(stamp.unsafe_fixnum());
  } else if (gc::IsA<core::FuncallableInstance_sp>(obj)) {
    core::FuncallableInstance_sp iobj = gc::As_unsafe<core::FuncallableInstance_sp>(obj);
    return iobj->stamp_set(stamp.unsafe_fixnum());
  }
  SIMPLE_ERROR("Only Instance and FuncallableInstance objects can have their stamp set", _rep_(obj));
}

CL_LAMBDA(obj);
CL_DOCSTRING(R"dx(Return true if the object inherits from core:instance based on its header value)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__inherits_from_instance(core::T_sp obj) { return (gc::IsA<core::Instance_sp>(obj)); }

}; // namespace gctools

namespace gctools {

CL_LAMBDA(on &key (backtrace-start 0) (backtrace-count 0) (backtrace-depth 6));
DOCGROUP(clasp);
CL_DEFUN void gctools__monitor_allocations(bool on, core::Fixnum_sp backtraceStart, core::Fixnum_sp backtraceCount,
                                           core::Fixnum_sp backtraceDepth) {
#ifdef DEBUG_MONITOR_ALLOCATIONS
  global_monitorAllocations.on = on;
  global_monitorAllocations.counter = 0;
  if (backtraceStart.unsafe_fixnum() < 0 || backtraceCount.unsafe_fixnum() < 0 || backtraceDepth.unsafe_fixnum() < 0) {
    SIMPLE_ERROR("Keyword arguments must all be >= 0");
  }
  global_monitorAllocations.start = backtraceStart.unsafe_fixnum();
  global_monitorAllocations.end = backtraceStart.unsafe_fixnum() + backtraceCount.unsafe_fixnum();
  global_monitorAllocations.backtraceDepth = backtraceDepth.unsafe_fixnum();
  printf("%s:%d  monitorAllocations set to %d\n", __FILE__, __LINE__, on);
#endif
};

SYMBOL_EXPORT_SC_(GcToolsPkg, ramp);
SYMBOL_EXPORT_SC_(GcToolsPkg, rampCollectAll);

DOCGROUP(clasp);
CL_DEFUN void gctools__alloc_pattern_begin(core::Symbol_sp pattern) {
  // to be implemented - based on old MPS code that would
  // alert the GC about the pattern of memory allocations coming up.
};

DOCGROUP(clasp);
CL_DEFUN core::Symbol_sp gctools__alloc_pattern_end() {
  return nil<core::Symbol_O>();
};

}; // namespace gctools

namespace gctools {

SYMBOL_EXPORT_SC_(KeywordPkg, test);

inline RoomVerbosity roomVerbosity(core::Symbol_sp x) {
  if (x == cl::_sym_T) return room_max;
  else if (x == kw::_sym_default) return room_default;
  else if (x == kw::_sym_test) return room_test;
  else if (x.nilp()) return room_min;
  else
    TYPE_ERROR(x, core::Cons_O::createList(cl::_sym_member, cl::_sym_T, cl::_sym_nil, kw::_sym_default));
}

size_t dumpReachableClassMap(const gctools::ReachableClassMap& data, std::ostream& OutputStream) {
  // note: can't use RCMap::value_type as that has const key,
  // rendering the vector unsortable.
  vector<std::pair<gctools::GCStampEnum, gctools::ReachableClass>> values;
  for (auto it : data) {
    values.push_back(it); // copy for sorting
  }
  size_t totalSize = 0;
  sort(values.begin(), values.end(),
       [](auto& x, auto& y) {
         return (x.second.totalSize > y.second.totalSize);
       });
  for (auto it : values) {
    Fixnum k = it.first;
    const gctools::ReachableClass& rc = it.second;
    string className;
    if (k > 0 && k <= gctools::STAMPWTAG_max) {
      const char* nm = obj_name(k);
      if (nm) {
        className = nm;
        if (className.size() > 10) className = className.substr(10);
      } else className = "NULL-NAME";
    } else className = "UNKNOWN";
    totalSize += rc.totalSize;
    OutputStream << fmt::format("total_size: {:10d} count: {:8d} avg.sz: {:8d} {}/{}\n", rc.totalSize, rc.instances,
                                (rc.totalSize / rc.instances), className, k);
  }
  return totalSize;
}

void displayClassKinds(const ReachableClassMap& rcmap, std::ostream& OutputStream) {
  OutputStream << "-------------------- Reachable ClassKinds -------------------\n";
  dumpReachableClassMap(rcmap, OutputStream);
  OutputStream << "Done walk of memory  " << static_cast<uintptr_t>(rcmap.size()) << " ClassKinds\n";
}

struct RoomSummary {
  size_t consTotalSize = 0;
  size_t consCount = 0;
  size_t otherTotalSize = 0;
  size_t otherCount = 0;
};

RoomSummary summarizeResults(const gctools::ReachableClassMap& data) {
  RoomSummary summary;
  for (auto it : data) {
    Fixnum k = it.first;
    gctools::ReachableClass rc = it.second;
    size_t sz = rc.totalSize;

    if (k == STAMP_UNSHIFT_WTAG(gctools::STAMPWTAG_core__Cons_O)) {
      summary.consCount += rc.instances;
      summary.consTotalSize += sz;
    } else {
      summary.otherCount += rc.instances;
      summary.otherTotalSize += sz;
    }
  }
  return summary;
}

void displayClassKindsSummary(const ReachableClassMap& rcmap, std::ostream& OutputStream) {
  RoomSummary summary = summarizeResults(rcmap);
  size_t totalSize = summary.consTotalSize + summary.otherTotalSize;
  OutputStream << fmt::format("Walk of memory found {} different classes.\n",
                              static_cast<uintptr_t>(rcmap.size()));
  OutputStream << fmt::format("There are {} cons objects occupying {} bytes {:4.1f}% of memory.\n", summary.consCount,
                              summary.consTotalSize, (float)summary.consTotalSize / totalSize * 100.0);
  OutputStream << fmt::format("There are {} other objects occupying {} bytes {:4.1f}% of memory.\n", summary.otherCount,
                              summary.otherTotalSize, (float)summary.otherTotalSize / totalSize * 100.0);
}

void roomMapper(Tagged tagged, void* data) {
  ReachableClassMap* rcmap = (ReachableClassMap*)data;
  uintptr_t tag = tagged & ptag_mask;
  BaseHeader_s* header;
  GCStampEnum stamp;

  if (tag == general_tag) {
    uintptr_t obj = tagged & ptr_mask;
    header = (Header_s*)GeneralPtrToHeaderPtr((void*)obj);
    stamp = header->_badge_stamp_wtag_mtag.stamp_();
  } else if (tag == cons_tag) {
    uintptr_t obj = tagged & ptr_mask;
    header = (ConsHeader_s*)ConsPtrToHeaderPtr((void*)obj);
    stamp = (gctools::GCStampEnum)STAMP_UNSHIFT_WTAG(gctools::STAMPWTAG_CONS);
  } else return; // immediate or vaslist

  size_t sz = objectSize(header);

  (*rcmap)[stamp].update(sz);
}

void* map_gc_objects_w_alloc_lock(void* data) {
  mapAllObjects(roomMapper, data);
  return nullptr;
}

void fill_reachable_class_map(ReachableClassMap* rcmap) {
  call_with_stopped_world(map_gc_objects_w_alloc_lock,
                          (void*)rcmap);
}

CL_LAMBDA(&optional (x :default));
CL_DEFUN void cl__room(core::Symbol_sp x) {
  std::ostringstream OutputStream;
  RoomVerbosity verb = roomVerbosity(x);
  // With precise GC, we can get per-class counts.
#ifdef USE_PRECISE_GC
  switch (verb) {
  case room_max: {
    ReachableClassMap rcmap;
    fill_reachable_class_map(&rcmap);
    displayClassKinds(rcmap, OutputStream);
  } break;
  case room_default: {
    ReachableClassMap rcmap;
    fill_reachable_class_map(&rcmap);
    displayClassKindsSummary(rcmap, OutputStream);
  } break;
  default: break; // save some effort by not even computing the map
  }
#endif

  // Summary statistics
  OutputStream << "Total heap bytes:                              " << std::setw(12) << heap_size() << '\n';
  OutputStream << "Free bytes:                                    " << std::setw(12) << free_bytes() << '\n';
  OutputStream << "Bytes allocated since last GC:                 " << std::setw(12) << bytes_since_gc() << '\n';

  // Write it all out.  
  clasp_write_string(OutputStream.str(), cl::_sym_STARstandard_outputSTAR->symbolValue());
}

}; // namespace gctools

namespace gctools {

CL_LAMBDA(filename &key executable test-memory);
CL_DECLARE();
CL_DOCSTRING(R"dx(Save a snapshot, i.e. enough information to restart a Lisp process
later in the same state, in the file of the specified name. Only
global state is preserved. After the snapshot is saved, clasp will exit.

The following &KEY arguments are defined:
  :EXECUTABLE
     If true, arrange to combine the Clasp runtime and the snapshot
     to create a standalone executable.  If false (the default), the
     snapshot will not be executable on its own.
  :TEST-MEMORY
     Test memory prior to saving snapshot.
     If NIL then snapshot saving is faster.)dx")
DOCGROUP(clasp);
CL_DEFUN void gctools__save_lisp_and_die(core::T_sp filename, core::T_sp executable, core::T_sp testMemory) {
#ifdef USE_PRECISE_GC
  throw(core::SaveLispAndDie(gc::As<core::String_sp>(filename)->get_std_string(), executable.notnilp(),
                             globals_->_Bundle->_Directories->_LibDir, true, core::noStomp, testMemory.notnilp() ));
#else
  SIMPLE_ERROR("save-lisp-and-die only works for precise GC");
#endif
}

CL_LAMBDA(filename &key executable);
CL_DECLARE();
CL_DOCSTRING(R"dx(Save a snapshot, i.e. enough information to restart a Lisp process
later in the same state, in the file of the specified name. Only
global state is preserved. After the snapshot is saved, clasp will continue running!
THIS IS EXPERIMENTAL - I am not yet sure if it's safe to keep running clasp!

The following &KEY arguments are defined:
  :EXECUTABLE
     If true, arrange to combine the Clasp runtime and the snapshot
     to create a standalone executable.  If false (the default), the
     snapshot will not be executable on its own.)dx")
DOCGROUP(clasp);
CL_DEFUN void gctools__save_lisp_and_continue(core::T_sp filename, core::T_sp executable) {
#ifdef USE_PRECISE_GC
  core::SaveLispAndDie ee(gc::As<core::String_sp>(filename)->get_std_string(), executable.notnilp(),
                          globals_->_Bundle->_Directories->_LibDir, false );
  snapshotSaveLoad::snapshot_save(ee);
#else
  SIMPLE_ERROR("save-lisp-and-continue only works for precise GC");
#endif
}
}; // namespace gctools

namespace gctools {
/*! Call finalizer_callback with no arguments when object is finalized.*/
DOCGROUP(clasp);
CL_DEFUN void gctools__finalize(core::T_sp object, core::T_sp finalizer_callback) {
  // printf("%s:%d making a finalizer for %p calling %p\n", __FILE__, __LINE__, (void*)object.tagged_(),
  // (void*)finalizer_callback.tagged_());
  WITH_READ_WRITE_LOCK(globals_->_FinalizersMutex);
  core::WeakKeyHashTable_sp ht = _lisp->_Roots._Finalizers;
  core::List_sp orig_finalizers = ht->gethash(object, nil<core::T_O>());
  core::List_sp finalizers = core::Cons_O::create(finalizer_callback, orig_finalizers);
  //  printf("%s:%d      Adding finalizer to list new length --> %lu   list head %p\n", __FILE__, __LINE__,
  //  core::cl__length(finalizers), (void*)finalizers.tagged_());
  ht->hash_table_setf_gethash(object, finalizers);
  // Register the finalizer with the GC
#if defined(USE_BOEHM)
  boehm_set_finalizer_list(object.tagged_(), finalizers.tagged_());
#elif defined(USE_MMTK)
  MISSING_GC_SUPPORT();
#endif
};

DOCGROUP(clasp);
CL_DEFUN core::T_sp gctools__finalizers(core::T_sp object) {
  WITH_READ_WRITE_LOCK(globals_->_FinalizersMutex);
  core::WeakKeyHashTable_sp ht = _lisp->_Roots._Finalizers;
  return ht->gethash(object, nil<core::T_O>());
}

DOCGROUP(clasp);
CL_DEFUN int gctools__invoke_finalizers() {
#if defined(USE_BOEHM)
  return GC_invoke_finalizers();
#else
  MISSING_GC_SUPPORT();
#endif
}

DOCGROUP(clasp);
CL_DEFUN void gctools__definalize(core::T_sp object) {
  //  printf("%s:%d erasing finalizers for %p\n", __FILE__, __LINE__, (void*)object.tagged_());
  WITH_READ_WRITE_LOCK(globals_->_FinalizersMutex);
  core::WeakKeyHashTable_sp ht = _lisp->_Roots._Finalizers;
  if (ht->gethash(object))
    ht->remhash(object);
#if defined(USE_BOEHM)
  boehm_clear_finalizer_list(object.tagged_());
#else
  MISSING_GC_SUPPORT();
#endif
}

}; // namespace gctools

namespace gctools {

DOCGROUP(clasp);
CL_DEFUN core::T_mv gctools__memory_profile_status() {
  int64_t allocationNumberCounter = my_thread_low_level->_Allocations._AllocationNumberCounter;
  size_t allocationNumberThreshold = my_thread_low_level->_Allocations._AllocationNumberThreshold;
  int64_t allocationSizeCounter = my_thread_low_level->_Allocations._AllocationSizeCounter;
  size_t allocationSizeThreshold = my_thread_low_level->_Allocations._AllocationSizeThreshold;
  return Values(core::make_fixnum(allocationNumberCounter), core::make_fixnum(allocationNumberThreshold),
                core::make_fixnum(allocationSizeCounter), core::make_fixnum(allocationSizeThreshold));
}


CL_DEFUN core::T_sp gctools__object_address(core::General_sp generalObject) {
  void* address = (void*)&(*generalObject);
  return core::Integer_O::create((uint64_t)address);
}

CL_DEFUN core::T_sp gctools__vtable_address(core::General_sp generalObject) {
  void* vtable_ptr = *(void**)&(*generalObject);
  printf("%s:%d:%s  vtable pointer = %p\n", __FILE__, __LINE__, __FUNCTION__, vtable_ptr );
  return nil<core::T_O>();
}

DOCGROUP(clasp);
CL_DEFUN void gctools__garbage_collect() {
#if defined(USE_BOEHM)
  GC_gcollect();
  GC_invoke_finalizers();
#else
  MISSING_GC_SUPPORT();
#endif
  //        printf("Garbage collection done\n");
};

DOCGROUP(clasp);
CL_DEFUN void gctools__register_stamp_name(const std::string& name, size_t stamp_num) { register_stamp_name(name, stamp_num); }

DOCGROUP(clasp);
CL_DEFUN core::T_sp gctools__get_stamp_name_map() {
  core::List_sp l = nil<core::T_O>();
  for (auto it : global_unshifted_nowhere_stamp_name_map) {
    l = core::Cons_O::create(core::Cons_O::create(core::SimpleBaseString_O::make(it.first), core::make_fixnum(it.second)), l);
  }
  return l;
}

#define ARGS_gctools__debug_allocations "(arg)"
#define DECL_gctools__debug_allocations ""
#define DOCS_gctools__debug_allocations "debugAllocations"
DOCGROUP(clasp);
CL_DEFUN void gctools__debug_allocations(core::T_sp debugOn) { _GlobalDebugAllocations = debugOn.isTrue(); };

bool debugging_configuration(bool setFeatures, bool buildReport, stringstream& ss) {
  core::List_sp features = cl::_sym_STARfeaturesSTAR->symbolValue();
  bool debugging = false;

  bool use_symbols_in_global_array = false;
#ifdef USE_SYMBOLS_IN_GLOBAL_ARRAY
  use_symbols_in_global_array = true;
#endif
  if (buildReport)
    ss << (fmt::format("USE_SYMBOLS_IN_GLOBAL_ARRAY = {}\n", (use_symbols_in_global_array ? "**DEFINED**" : "undefined")));

  bool use_static_analyzer_global_symbols = false;
#ifdef USE_STATIC_ANALYZER_GLOBAL_SYMBOLS
  use_static_analyzer_global_symbols = true;
#endif
  if (buildReport)
    ss << (fmt::format("USE_STATIC_ANALYZER_GLOBAL_SYMBOLS = {}\n",
                       (use_static_analyzer_global_symbols ? "**DEFINED**" : "undefined")));

  bool debug_throw_if_invalid_client_on = false;
#ifdef DEBUG_THROW_IF_INVALID_CLIENT_ON
  debug_throw_if_invalid_client_on = true;
  debugging = true;
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_THROW_IF_INVALID_CLIENT_ON = {}\n",
                       (debug_throw_if_invalid_client_on ? "**DEFINED**" : "undefined")));

  bool debug_telemetry = false;
#ifdef DEBUG_TELEMETRY
  debug_telemetry = true;
  debugging = true;
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_TELEMETRY = {}\n", (debug_telemetry ? "**DEFINED**" : "undefined")));

  bool debug_alloc_alignment = false;
#ifdef DEBUG_ALLOC_ALIGNMENT
  debug_alloc_alignment = true;
  debugging = true;
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_ALLOC_ALIGNMENT = {}\n", (debug_alloc_alignment ? "**DEFINED**" : "undefined")));

  bool debug_stackmaps = false;
#ifdef DEBUG_STACKMAPS
  debug_stackmaps = true;
  debugging = true;
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_STACKMAPS = {}\n", (debug_stackmaps ? "**DEFINED**" : "undefined")));

  bool debug_stack_telemetry = false;
#ifdef DEBUG_STACK_TELEMETRY
  debug_stack_telemetry = true;
  debugging = true;
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_STACK_TELEMETRY = {}\n", (debug_stack_telemetry ? "**DEFINED**" : "undefined")));

  bool debug_recursive_allocations = false;
#ifdef DEBUG_RECURSIVE_ALLOCATIONS
  debug_recursive_allocations = true;
  debugging = true;
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_RECURSIVE_ALLOCATIONS = {}\n", (debug_recursive_allocations ? "**DEFINED**" : "undefined")));

  bool config_var_cool = false;
#ifdef CONFIG_VAR_COOL
  config_var_cool = true;
  debugging = true;
#endif
  if (buildReport)
    ss << (fmt::format("CONFIG_VAR_COOL = {}\n", (config_var_cool ? "**DEFINED**" : "undefined")));

  bool debug_guard = false;
#ifdef DEBUG_GUARD
  debug_guard = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-GUARD"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_GUARD = {}\n", (debug_guard ? "**DEFINED**" : "undefined")));

  bool debug_guard_backtrace = false;
#ifdef DEBUG_GUARD_BACKTRACE
  debug_guard_backtrace = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-GUARD-BACKTRACE"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_GUARD_BACKTRACE = {}\n", (debug_guard_backtrace ? "**DEFINED**" : "undefined")));

  bool debug_validate_guard = false;
#ifdef DEBUG_VALIDATE_GUARD
  debug_validate_guard = true;
  debugging = true;
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_VALIDATE_GUARD = {}\n", (debug_validate_guard ? "**DEFINED**" : "undefined")));

  bool debug_ensure_valid_object = false;
#ifdef DEBUG_ENSURE_VALID_OBJECT
  debug_ensure_valid_object = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-ENSURE-VALID-OBJECT"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_ENSURE_VALID_OBJECT = {}\n", (debug_ensure_valid_object ? "**DEFINED**" : "undefined")));

  bool debug_cache = false;
#ifdef DEBUG_CACHE
  debug_cache = true;
  debugging = true;
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_CACHE = {}\n", (debug_cache ? "**DEFINED**" : "undefined")));

  bool debug_threads = false;
#ifdef DEBUG_THREADS
  debug_threads = true;
  debugging = true;
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_THREADS = {}\n", (debug_threads ? "**DEFINED**" : "undefined")));

  bool debug_gfdispatch = false;
#ifdef DEBUG_GFDISPATCH
  debug_gfdispatch = true;
  debugging = true;
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_GFDISPATCH = {}\n", (debug_gfdispatch ? "**DEFINED**" : "undefined")));

  bool debug_cst = false;
#ifdef CST
  debug_cst = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("CST"), features);
#endif
  if (buildReport)
    ss << (fmt::format("CST = {}\n", (debug_gfdispatch ? "**DEFINED**" : "undefined")));

  bool debug_ihs = false;
#ifdef DEBUG_IHS
  debug_ihs = true;
  debugging = true;
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_IHS = {}\n", (debug_ihs ? "**DEFINED**" : "undefined")));

  bool debug_enable_profiling = false;
#ifdef ENABLE_PROFILING
  debug_enable_profiling = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("ENABLE-PROFILING"), features);
#endif
  if (buildReport)
    ss << (fmt::format("ENABLE_PROFILING = {}\n", (debug_enable_profiling ? "**DEFINED**" : "undefined")));

  bool debug_release = false;
#ifdef DEBUG_RELEASE
  debug_release = true;
  debugging = true;
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_RELEASE = {}\n", (debug_release ? "**DEFINED**" : "undefined")));

  bool debug_bounds_assert = false;
#ifdef DEBUG_BOUNDS_ASSERT
  debug_bounds_assert = true;
  debugging = true;
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_BOUNDS_ASSERT = {}\n", (debug_bounds_assert ? "**DEFINED**" : "undefined")));

  bool debug_slot_accessors = false;
#ifdef DEBUG_SLOT_ACCESSORS
  debug_slot_accessors = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-SLOT-ACCESSORS"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_SLOT_ACCESSORS = {}\n", (debug_slot_accessors ? "**DEFINED**" : "undefined")));

  bool debug_fastgf = false;
#ifdef DEBUG_FASTGF
  debug_fastgf = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-FASTGF"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_FASTGF = {}\n", (debug_fastgf ? "**DEFINED**" : "undefined")));

  bool debug_rehash_count = false;
#ifdef DEBUG_REHASH_COUNT
#ifndef DEBUG_MONITOR
#error "DEBUG_MONITOR must also be enabled if DEBUG_REHASH_COUNT is turned on"
#endif
  debug_rehash_count = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-REHASH_COUNT"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_REHASH_COUNT = {}\n", (debug_rehash_count ? "**DEFINED**" : "undefined")));

  bool debug_jit_log_symbols = false;
#ifdef DEBUG_JIT_LOG_SYMBOLS
  if (!core::global_options->_SilentStartup) {
    printf("%s:%d  Setting JIT-LOG-SYMBOLS *feature*\n", __FILE__, __LINE__);
  }
  debug_jit_log_symbols = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("JIT-LOG-SYMBOLS"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_JIT_LOG_SYMBOLS = {}\n", (debug_jit_log_symbols ? "**DEFINED**" : "undefined")));

  bool sanitize_memory = false;
#ifdef SANITIZE_MEMORY
  sanitize_memory = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("SANITIZE-MEMORY"), features);
#endif
  if (buildReport)
    ss << (fmt::format("SANITIZE_MEMORY = {}\n", (sanitize_memory ? "**DEFINED**" : "undefined")));

  bool debug_bclasp_lisp = false;
#ifdef DEBUG_BCLASP_LISP
  debug_bclasp_lisp = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-BCLASP-LISP"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_BCLASP_LISP = {}\n", (debug_bclasp_lisp ? "**DEFINED**" : "undefined")));

  bool track_allocations = false;
#ifdef DEBUG_TRACK_ALLOCATIONS
  track_allocations = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("TRACK-ALLOCATIONS"), features);
#endif
  if (buildReport)
    ss << (fmt::format("TRACK_ALLOCATIONS = {}\n", (track_allocations ? "**DEFINED**" : "undefined")));

  bool debug_dtree_interpreter = false;
#ifdef DEBUG_DTREE_INTERPRETER
  debug_dtree_interpreter = true;
#ifndef DEBUG_MONITOR_SUPPORT
#error "You must enable DEBUG_MONITOR_SUPPORT to use DEBUG_DTREE_INTERPRETER"
#endif
  debugging = true;
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_DTREE_INTERPRETER = {}\n", (debug_dtree_interpreter ? "**DEFINED**" : "undefined")));

  bool debug_cclasp_lisp = false;
#ifdef DEBUG_CCLASP_LISP
  debug_cclasp_lisp = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-CCLASP-LISP"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_CCLASP_LISP = {}\n", (debug_cclasp_lisp ? "**DEFINED**" : "undefined")));

  bool debug_long_call_history = false;
#ifdef DEBUG_LONG_CALL_HISTORY
  debug_long_call_history = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-LONG-CALL-HISTORY"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_LONG_CALL_HISTORY = {}\n", (debug_long_call_history ? "**DEFINED**" : "undefined")));

  bool debug_memory_profile = false;
#ifdef DEBUG_MEMORY_PROFILE
  debug_memory_profile = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-MEMORY-PROFILE"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_MEMORY_PROFILE = {}\n", (debug_memory_profile ? "**DEFINED**" : "undefined")));

  bool debug_compiler = false;
#ifdef DEBUG_COMPILER
  debug_compiler = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-COMPILER"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_COMPILER = {}\n", (debug_compiler ? "**DEFINED**" : "undefined")));

  bool debug_verify_modules = false;
#ifdef DEBUG_VERIFY_MODULES
  debug_verify_modules = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-VERIFY-MODULES"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_VERIFY_MODULES = {}\n", (debug_verify_modules ? "**DEFINED**" : "undefined")));

  bool debug_verify_transformations = false;
#ifdef DEBUG_VERIFY_TRANSFORMATIONS
  debug_verify_transformations = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-VERIFY-TRANSFORMATIONS"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_VERIFY_TRANSFORMATIONS = {}\n", (debug_verify_transformations ? "**DEFINED**" : "undefined")));

  bool debug_assert_type_cast = false;
#ifdef DO_ASSERT_TYPE_CAST
  debug_assert_type_cast = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-ASSERT-TYPE-CAST"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DO_ASSERT_TYPE_CAST = {}\n", (debug_assert_type_cast ? "**DEFINED**" : "undefined")));

  bool debug_llvm_optimization_level_0 = false;
#ifdef DEBUG_LLVM_OPTIMIZATION_LEVEL_0
  debug_llvm_optimization_level_0 = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-LLVM-OPTIMIZATION-LEVEL-0"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_LLVM_OPTIMIZATION_LEVEL_0 = {}\n", (debug_llvm_optimization_level_0 ? "**DEFINED**" : "undefined")));

  bool debug_dont_optimize_bclasp = false;
#ifdef DEBUG_DONT_OPTIMIZE_BCLASP
  debug_dont_optimize_bclasp = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-DONT-OPTIMIZE-BCLASP"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_DONT_OPTIMIZE_BCLASP = {}\n", (debug_dont_optimize_bclasp ? "**DEFINED**" : "undefined")));

  bool debug_dtrace_lock_probe = false;
#ifdef DEBUG_DTRACE_LOCK_PROBE
  debug_dtrace_lock_probe = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-DTRACE-LOCK-PROBE"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_DTRACE_LOCK_PROBE = {}\n", (debug_dtrace_lock_probe ? "**DEFINED**" : "undefined")));

  bool debug_stores = false;
#ifdef DEBUG_STORES
  debug_stores = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-STORES"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_STORES = {}\n", (debug_stores ? "**DEFINED**" : "undefined")));

  bool disable_type_inference = false;
#ifdef DISABLE_TYPE_INFERENCE
  disable_type_inference = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DISABLE-TYPE-INFERENCE"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DISABLE_TYPE_INFERENCE = {}\n", (disable_type_inference ? "**DEFINED**" : "undefined")));

#if USE_COMPILE_FILE_PARALLEL == 0
  use_compile_file_parallel = false;
  INTERN_(comp, STARuse_compile_file_parallelSTAR)->defparameter(nil<core::T_O>());
  printf("%s:%d You have turned off compile-file-parallel\n   - you can enable it by setting USE_COMPILE_FILE_PARALLEL in the "
         "wscript.config\n   - compile-file-parallel should be enabled by default\n",
         __FILE__, __LINE__);
#else
  INTERN_(comp, STARuse_compile_file_parallelSTAR)->defparameter(_lisp->_true());
#endif
  if (buildReport)
    ss << (fmt::format("USE_COMPILE_FILE_PARALLEL = {}\n", USE_COMPILE_FILE_PARALLEL));

#if FORCE_STARTUP_EXTERNAL_LINKAGE == 0
  force_startup_external_linkage = false;
  INTERN_(comp, STARforce_startup_external_linkageSTAR)->defparameter(nil<core::T_O>());
#else
  INTERN_(comp, STARforce_startup_external_linkageSTAR)->defparameter(_lisp->_true());
#endif
  if (buildReport)
    ss << (fmt::format("FORCE_STARTUP_EXTERNAL_LINKAGE = {}\n", FORCE_STARTUP_EXTERNAL_LINKAGE));

  bool use_human_readable_bitcode = false;
#if USE_HUMAN_READABLE_BITCODE == 1
  use_human_readable_bitcode = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("USE-HUMAN-READABLE-BITCODE"), features);
#endif
  if (buildReport)
    ss << (fmt::format("USE_HUMAN_READABLE_BITCODE = {}\n", (use_human_readable_bitcode ? "**DEFINED**" : "undefined")));

  bool debug_compile_file_output_info = false;
#if DEBUG_COMPILE_FILE_OUTPUT_INFO == 1
  debug_compile_file_output_info = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-COMPILE-FILE-OUTPUT-INFO"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_COMPILE_FILE_OUTPUT_INFO = {}\n", (debug_compile_file_output_info ? "**DEFINED**" : "undefined")));

  bool debug_dyn_env_stack = false;
#if DEBUG_DYN_ENV_STACK == 1
  debug_dyn_env_stack = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-DYN-ENV-STACK"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_DYN_ENV_STACK = {}\n", (debug_dyn_env_stack ? "**DEFINED**" : "undefined")));

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
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-MONITOR"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_MONITOR = {}\n", (debug_monitor ? "**DEFINED**" : "undefined")));

  bool debug_monitor_support = false;
#ifdef DEBUG_MONITOR_SUPPORT
  debug_monitor_support = true;
  debugging = true;
  if (setFeatures)
    features = core::Cons_O::create(_lisp->internKeyword("DEBUG-MONITOR-SUPPORT"), features);
#endif
  if (buildReport)
    ss << (fmt::format("DEBUG_MONITOR_SUPPORT = {}\n", (debug_monitor_support ? "**DEFINED**" : "undefined")));

  // -------------------------------------------------------------
  //
  // The cl:*features* environment variable is set below - so all changes to (features) must be above
  //
  if (setFeatures)
    cl::_sym_STARfeaturesSTAR->setf_symbolValue(features);

  // ---- return with the debugging flag
  return debugging;
}

DOCGROUP(clasp);
CL_DEFUN void gctools__configuration() {
  stringstream ss;
  [[maybe_unused]] bool debugging = debugging_configuration(false, true, ss);
  core::clasp_writeln_string(ss.str());
}

DOCGROUP(clasp);
CL_DEFUN void gctools__thread_local_cleanup() {
#ifdef DEBUG_FINALIZERS
  printf("%s:%d:%s  called to cleanup thread local resources\n", __FILE__, __LINE__, __FUNCTION__);
#endif
  core::thread_local_invoke_and_clear_cleanup();
}

DOCGROUP(clasp);
CL_DEFUN core::Integer_sp gctools__unwind_time_nanoseconds() {
  core::Integer_sp is = core::Integer_O::create(my_thread_low_level->_unwind_time.count());
  return is;
}

}; // namespace gctools
