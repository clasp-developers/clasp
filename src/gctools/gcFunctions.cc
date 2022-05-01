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
#include <clasp/core/pathname.h>
#include <clasp/core/activationFrame.h>
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
#include <clasp/core/wrappers.h>


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

DOCGROUP(clasp)
CL_DEFUN void gctools__accumulate_discriminating_function_compilation_seconds(double seconds) {
  global_DiscriminatingFunctionCompilationSeconds =
      global_DiscriminatingFunctionCompilationSeconds + seconds;
}

DOCGROUP(clasp)
CL_DEFUN double gctools__discriminating_function_compilation_seconds() {
  return global_DiscriminatingFunctionCompilationSeconds;
}

};


namespace gctools {


size_t global_next_unused_kind = STAMPWTAG_max+1;

/*! Hardcode a few kinds of objects for bootstrapping
 */

const char *global_HardcodedKinds[] = {
    "", "core::T_O", "core::Instance_O", "core::Symbol_O"};

DOCGROUP(clasp)
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

DOCGROUP(clasp)
CL_DEFUN core::T_sp gctools__next_lexical_depth_counter() {
  core::T_sp result = core::make_fixnum(++global_lexical_depth_counter);
  return result;
}

#ifdef DEBUG_FLOW_TRACKER
DOCGROUP(clasp)
CL_DEFUN void gctools__flow_tracker_on() {
  initialize_flow_tracker();
  global_flow_tracker_on = true;
}

DOCGROUP(clasp)
CL_DEFUN void gctools__flow_tracker_off() {
  global_flow_tracker_on = false;
  flow_tracker_close();
}
#endif

DOCGROUP(clasp)
CL_DEFUN core::Cons_sp gctools__bootstrap_kind_symbols() {
  core::Cons_sp list(nil<core::Cons_O>());
  for (int i(gctools__max_bootstrap_kinds() - 1); i > 0; --i) {
    string name = global_HardcodedKinds[i];
    list = core::Cons_O::create(core::SimpleBaseString_O::make(name), list);
  }
  return list;
}

DOCGROUP(clasp)
CL_DEFUN core::T_sp gctools__bootstrap_kind_p(const string &name) {
  for (int i(0), iEnd(gctools__max_bootstrap_kinds()); i < iEnd; ++i) {
    //            printf("%s:%d i[%d]Checking if %s == %s\n", __FILE__, __LINE__, i, global_HardcodedKinds[i], name.c_str());
    if (strcmp(global_HardcodedKinds[i], name.c_str()) == 0) {
      return core::make_fixnum(i);
    }
  }
  return nil<core::T_O>();
}

DOCGROUP(clasp)
CL_DEFUN size_t gctools__thread_local_unwind_counter() {
  return my_thread->_unwinds;
}

DOCGROUP(clasp)
CL_DEFUN void gctools__change_sigchld_sigport_handlers()
{
  void* old = (void*)signal(SIGCHLD, SIG_DFL);
  signal(SIGPIPE, SIG_DFL);
  printf("%s:%d old signal handler for SIGCHLD = %p   SIG_DFL = %p\n", __FILE__, __LINE__, old, SIG_DFL);
}

DOCGROUP(clasp)
CL_DEFUN core::T_sp gctools__unix_signal_handlers()
{
  return _lisp->_Roots._UnixSignalHandlers;
}

DOCGROUP(clasp)
CL_DEFUN void gctools__deallocate_unmanaged_instance(core::T_sp obj) {
  obj_deallocate_unmanaged_instance(obj);
}

CL_DOCSTRING(R"dx(Return bytes allocated (values clasp-calculated-bytes))dx")
DOCGROUP(clasp)
CL_DEFUN core::T_sp gctools__bytes_allocated() {
  size_t my_bytes = my_thread_low_level->_Allocations._BytesAllocated;
  ASSERT(my_bytes < gc::most_positive_fixnum);
  return core::clasp_make_fixnum(my_bytes);
}



CL_DOCSTRING(R"dx(Return the next unused kind)dx")
DOCGROUP(clasp)
CL_DEFUN size_t core__next_unused_kind() {
  size_t next = global_next_unused_kind;
  ++global_next_unused_kind;
  return next;
}

};


namespace gctools {

CL_DOCSTRING(R"dx(Return the header value for the object)dx")
DOCGROUP(clasp)
CL_DEFUN core::T_sp core__header_value(core::T_sp obj) {
  if (obj.generalp()) {
    void *mostDerived = gctools::untag_general<void *>(obj.raw_());
    const gctools::Header_s *header = reinterpret_cast<const gctools::Header_s *>(gctools::GeneralPtrToHeaderPtr(mostDerived));
    return core::clasp_make_integer(header->_stamp_wtag_mtag._value);
  }
  SIMPLE_ERROR(BF("The object %s is not a general object and doesn't have a header-value") % _rep_(obj));
}


CL_DOCSTRING(R"dx(Return the header value for the object)dx")
DOCGROUP(clasp)
CL_DEFUN core::T_sp core__header_value_to_stamp(core::T_sp value) {
  if (value.fixnump()) {
    Fixnum fvalue = value.unsafe_fixnum();
    return core::make_fixnum(Header_s::value_to_stamp(fvalue));
  }
  TYPE_ERROR(value,cl::_sym_fixnum);
}

DOCGROUP(clasp)
CL_DEFUN core::T_mv gctools__tagged_pointer_mps_test()
{
  // Return the values used to identify tagged pointers (PTR&POINTER_TAG_MASK)==POINTER_TAG_EQ
  return Values(core::make_fixnum(POINTER_TAG_MASK),core::make_fixnum(POINTER_TAG_EQ));
}


CL_DOCSTRING(R"dx(Return the header kind for the object)dx")
DOCGROUP(clasp)
CL_DEFUN Fixnum core__header_kind(core::T_sp obj) {
  if (obj.consp()) {
    return gctools::STAMPWTAG_CONS;
  } else if (obj.fixnump()) {
    return gctools::STAMPWTAG_FIXNUM;
  } else if (obj.generalp()) {
    void *mostDerived = gctools::untag_general<void *>(obj.raw_());
    const gctools::Header_s *header = reinterpret_cast<const gctools::Header_s *>(gctools::GeneralPtrToHeaderPtr(mostDerived));
    gctools::GCStampEnum stamp = header->_stamp_wtag_mtag.stamp_();
    return (Fixnum)stamp;
  } else if (obj.single_floatp()) {
    return gctools::STAMPWTAG_SINGLE_FLOAT;
  } else if (obj.characterp()) {
    return gctools::STAMPWTAG_CHARACTER;
  } else if (obj.valistp()) {
    return gctools::STAMPWTAG_VASLIST_S;
  }
  printf("%s:%d HEADER-KIND requested for a non-general object - Clasp needs to define hard-coded kinds for non-general objects - returning -1 for now", __FILE__, __LINE__);
  SIMPLE_ERROR(BF("The object %s doesn't have a stamp") % _rep_(obj));
}

CL_DOCSTRING(R"dx(Return the index part of the stamp.  Stamp indices are adjacent to each other.)dx")
DOCGROUP(clasp)
CL_DEFUN size_t core__stamp_index(size_t stamp)
{
  return stamp>>(gctools::Header_s::wtag_width+gctools::Header_s::mtag_width);
}

CL_DOCSTRING(R"dx(Shift an unshifted stamp so that it can be put into code in a form where it can be directly matched to a stamp read from an object header with no further shifting)dx")
DOCGROUP(clasp)
CL_DEFUN core::Integer_sp core__shift_stamp_for_compiled_code(size_t unshifted_stamp)
{
  return core::make_fixnum((unshifted_stamp << gctools::Header_s::general_mtag_shift)
                           | gctools::Header_s::general_mtag);
}

CL_DOCSTRING(R"dx(Return the stamp for the object, the flags and the header stamp)dx")
DOCGROUP(clasp)
CL_DEFUN core::T_sp core__instance_stamp(core::T_sp obj)
{
  core::T_sp stamp((gctools::Tagged)cx_read_stamp(obj.raw_(),0));
  if (stamp.fixnump()) return stamp;
  SIMPLE_ERROR(BF("core:instance-stamp was about to return a non-fixnum %p") % (void*)stamp.raw_());
}

CL_DOCSTRING(R"dx(Return the tagged pointer for the object, the flags and the header stamp)dx")
DOCGROUP(clasp)
CL_DEFUN core::T_sp core__instance_tagged_pointer(core::T_sp obj)
{
  return core::Pointer_O::create(obj.raw_());
}

CL_DOCSTRING(R"dx(Determine if stamp A is immediately less than stamp B, so that they can be merged into a range.)dx")
DOCGROUP(clasp)
CL_DEFUN bool core__stamps_adjacent_p(size_t stamp_a, size_t stamp_b) {
  return (((stamp_a >> gctools::Header_s::general_mtag_shift) + 1)
          == (stamp_b >> gctools::Header_s::general_mtag_shift));
}

CL_DOCSTRING(R"dx(Set the header stamp for the object)dx")
DOCGROUP(clasp)
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


CL_LAMBDA(obj)
CL_DOCSTRING(R"dx(Return true if the object inherits from core:instance based on its header value)dx")
DOCGROUP(clasp)
CL_DEFUN bool core__inherits_from_instance(core::T_sp obj)
{
  return (gc::IsA<core::Instance_sp>(obj));
}

};

namespace gctools {

CL_LAMBDA(on &key (backtrace-start 0) (backtrace-count 0) (backtrace-depth 6))
DOCGROUP(clasp)
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

CL_LAMBDA(&optional marker)
DOCGROUP(clasp)
CL_DEFUN Fixnum gctools__gc_marker(core::Fixnum_sp marker) {
#if defined(USE_BOEHM)
# ifdef USE_BOEHM_MEMORY_MARKER
  if (marker.nilp()) {
    return gctools::globalBoehmMarker;
  }
  ASSERT(marker.fixnump());
  Fixnum oldm = gctools::globalBoehmMarker;
  Fixnum m = marker.unsafe_fixnum();
  gctools::globalBoehmMarker = m;
  return oldm;
# endif
#else
  MISSING_GC_SUPPORT();
#endif
  return 0;
}

SYMBOL_EXPORT_SC_(GcToolsPkg, STARallocPatternStackSTAR);
SYMBOL_EXPORT_SC_(GcToolsPkg, ramp);
SYMBOL_EXPORT_SC_(GcToolsPkg, rampCollectAll);

DOCGROUP(clasp)
CL_DEFUN void gctools__alloc_pattern_begin(core::Symbol_sp pattern) {
#if defined(USE_MPS)
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
  mps_ap_alloc_pattern_begin(my_thread_allocation_points._cons_allocation_point, mps_pat);
  mps_ap_alloc_pattern_begin(my_thread_allocation_points._automatic_mostly_copying_zero_rank_allocation_point,mps_pat);
#else
  // Do nothing
#endif
};

DOCGROUP(clasp)
CL_DEFUN core::Symbol_sp gctools__alloc_pattern_end() {
  core::Symbol_sp pattern(nil<core::Symbol_O>());
#if defined(USE_MPS)
  core::List_sp patternStack = gctools::_sym_STARallocPatternStackSTAR->symbolValue();
  if (patternStack.nilp())
    return nil<core::Symbol_O>();
  pattern = gc::As<core::Symbol_sp>(oCar(patternStack));
  gctools::_sym_STARallocPatternStackSTAR->setf_symbolValue(oCdr(patternStack));
  mps_alloc_pattern_t mps_pat;
  if (pattern == _sym_ramp) {
    mps_pat = mps_alloc_pattern_ramp();
  } else {
    mps_pat = mps_alloc_pattern_ramp_collect_all();
  }
  mps_ap_alloc_pattern_end(my_thread_allocation_points._automatic_mostly_copying_zero_rank_allocation_point,mps_pat);
  mps_ap_alloc_pattern_end(my_thread_allocation_points._cons_allocation_point, mps_pat);
  mps_ap_alloc_pattern_end(my_thread_allocation_points._automatic_mostly_copying_allocation_point, mps_pat);
#else
  // Do nothing
#endif
  return pattern;
};

};

#ifdef USE_MPS
struct MemoryMeasure {
  size_t _Count;
  size_t _Size;
  MemoryMeasure() : _Count(0), _Size(0) {};
};

struct MemoryCopy {
  uintptr_t _address;
  MemoryCopy(uintptr_t start) : _address(start) {};
};

extern "C" {

void amc_apply_measure(mps_addr_t client, void *p, size_t s) {
  MemoryMeasure* count = (MemoryMeasure*)p;
  count->_Count++;
  mps_addr_t next = obj_skip(client);
  count->_Size += ((size_t)next-(size_t)client);
}

void amc_apply_copy(mps_addr_t client, void* p, size_t s) {
  MemoryCopy* cpy = (MemoryCopy*)p;
  mps_addr_t next = obj_skip(client);
  size_t size = ((size_t)next-(size_t)client);
  void* header = reinterpret_cast<void*>(gctools::GeneralPtrToHeaderPtr(client));
  memcpy((void*)cpy->_address,(void*)header,size);
  cpy->_address += size;
};

};
#endif // USE_MPS


extern "C" {

#ifdef USE_MPS
#define SCAN_STRUCT_T int
#define ADDR_T mps_addr_t
#define SCAN_BEGIN(xxx)
#define SCAN_END(xxx)
#define POINTER_FIX(field)
#define EXTRA_ARGUMENTS
#define RESULT_TYPE    GC_RESULT
#define RESULT_OK MPS_RES_OK
#define OBJECT_SCAN fixup_objects
#define OBJECT_SKIP_IN_OBJECT_SCAN obj_skip_debug
#define GENERAL_PTR_TO_HEADER_PTR(_client_) gctools::GeneralPtrToHeaderPtr(_client_)
#include "obj_scan.cc"
#undef GENERAL_PTR_TO_HEADER_PTR
#undef RESULT_OK
#undef RESULT_TYPE   
#undef EXTRA_ARGUMENTS
#undef OBJ_SCAN
#undef SCAN_STRUCT_T
#endif // USE_MPS
};

namespace gctools {

#ifdef USE_MPS
void walk_memory(mps_pool_t pool, mps_amc_apply_stepper_t stepper, void* pdata, size_t psize) {
  mps_arena_park(global_arena);
  mps_amc_apply(pool, stepper, pdata, psize);
  mps_arena_release(global_arena);
}

DOCGROUP(clasp)
CL_DEFUN core::T_mv gctools__measure_memory() {
  MemoryMeasure count;
  walk_memory(global_amc_pool, amc_apply_measure,(void*)&count,sizeof(count));
  walk_memory(global_amcz_pool, amc_apply_measure,(void*)&count,sizeof(count));
  return Values(core::make_fixnum(count._Count),core::make_fixnum(count._Size));
}

DOCGROUP(clasp)
CL_DEFUN void gctools__copy_memory() {
  MemoryMeasure amc_measure;
  {
    walk_memory(global_amc_pool,amc_apply_measure,(void*)&amc_measure,sizeof(amc_measure));
    uintptr_t amc_buffer = (uintptr_t)malloc(amc_measure._Size+100000);
    MemoryCopy cpy(amc_buffer);
    walk_memory(global_amc_pool,amc_apply_copy,(void*)&cpy,sizeof(cpy));
    uintptr_t start = amc_buffer+sizeof(gctools::Header_s);
    uintptr_t stop = cpy._address+sizeof(gctools::Header_s);
    printf("%s:%d  fixup_objects from %p to %p\n", __FILE__, __LINE__, (void*)start, (void*)stop);
    // AMC pool needs fixup
    fixup_objects(0,(void*)start,(void*)stop);
    free((void*)amc_buffer);
  }
  // AMCZ pool doesn't need to be fixedup
  MemoryMeasure amcz_measure;
  {
    walk_memory(global_amcz_pool,amc_apply_measure,(void*)&amcz_measure,sizeof(amcz_measure));
    uintptr_t amcz_buffer = (uintptr_t)malloc(amcz_measure._Size+100000);
    MemoryCopy cpy(amcz_buffer);
    walk_memory(global_amcz_pool,amc_apply_copy,(void*)&cpy,sizeof(cpy));
    free((void*)amcz_buffer);
  }
  printf("%s:%d Copied and fixed up %lu bytes from AMC and copied %lu bytes from AMCZ\n",
         __FILE__, __LINE__, amc_measure._Size, amcz_measure._Size );
}
#endif // USE_MPS

CL_LAMBDA(&optional x)
CL_DECLARE();
CL_DOCSTRING(R"dx(room - Return info about the reachable objects in memory. x can be T, nil, :default.)dx")
DOCGROUP(clasp)
CL_DEFUN core::T_mv cl__room(core::T_sp x) {
  std::ostringstream OutputStream;
  gctools__garbage_collect();
  gctools__garbage_collect();
#if defined(USE_BOEHM) || defined(USE_MPS)
  clasp_gc_room(OutputStream);
#else
  MISSING_GC_SUPPORT();
#endif
  clasp_write_string(OutputStream.str(),cl::_sym_STARstandard_outputSTAR->symbolValue());
  return Values(nil<core::T_O>());
};
};

namespace gctools {

CL_LAMBDA(filename &key executable)
CL_DECLARE();
CL_DOCSTRING(R"dx(Save a snapshot, i.e. enough information to restart a Lisp process
later in the same state, in the file of the specified name. Only
global state is preserved.

The following &KEY arguments are defined:
  :EXECUTABLE
     If true, arrange to combine the Clasp runtime and the snapshot
     to create a standalone executable.  If false (the default), the
     snapshot will not be executable on its own.)dx")
DOCGROUP(clasp)
CL_DEFUN void gctools__save_lisp_and_die(core::T_sp filename, core::T_sp executable) {
#ifdef USE_PRECISE_GC
  throw(core::SaveLispAndDie(gc::As<core::String_sp>(filename)->get_std_string(), executable.notnilp(),
    globals_->_Bundle->_Directories->_LibDir));
#else
  SIMPLE_ERROR(BF("save-lisp-and-die only works for precise GC"));
#endif
}

CL_LAMBDA(stamp)
CL_DECLARE();
CL_DOCSTRING(R"dx(Return a list of addresses of objects with the given stamp)dx")
DOCGROUP(clasp)
CL_DEFUN core::T_sp gctools__objects_with_stamp(core::T_sp stamp) {
#if defined(USE_MPS)
  SIMPLE_ERROR(BF("Add support for MPS"));
#elif defined(USE_BOEHM)
  if (stamp.fixnump()) {
    gctools::FindStamp findStamp((gctools::GCStampEnum)stamp.unsafe_fixnum());
# if GC_VERSION_MAJOR >= 7 && GC_VERSION_MINOR >= 6
    GC_enumerate_reachable_objects_inner(boehm_callback_reachable_object_find_stamps, (void*)&findStamp);
# else
    SIMPLE_ERROR(BF("The boehm function GC_enumerate_reachable_objects_inner is not available"));
# endif
    core::List_sp result = nil<core::T_O>();
    for ( size_t ii=0; ii<findStamp._addresses.size(); ii++ ) {
      core::Pointer_sp ptr = core::Pointer_O::create((void*)findStamp._addresses[ii]);
      result = core::Cons_O::create(ptr,result);
    }
    return result;
  }
#else
  MISSING_GC_SUPPORT();
#endif // USE_BOEHM
  SIMPLE_ERROR(BF("You must pass a stamp value"));
}


CL_LAMBDA(address)
CL_DECLARE();
CL_DOCSTRING(R"dx(Return a list of addresses of objects with the given stamp)dx")
DOCGROUP(clasp)
CL_DEFUN core::T_sp gctools__objects_that_own(core::T_sp obj) {
#if defined(USE_MPS)
  SIMPLE_ERROR(BF("Add support for MPS"));
#elif defined(USE_BOEHM)
  if (obj.fixnump()) {
    void* base = GC_base((void*)obj.unsafe_fixnum());
    gctools::FindOwner findOwner(base);
# if GC_VERSION_MAJOR >= 7 && GC_VERSION_MINOR >= 6
    GC_enumerate_reachable_objects_inner(boehm_callback_reachable_object_find_owners, (void*)&findOwner);
# else
    SIMPLE_ERROR(BF("The boehm function GC_enumerate_reachable_objects_inner is not available"));
# endif
    core::List_sp result = nil<core::T_O>();
    for ( size_t ii=0; ii<findOwner._addresses.size(); ii++ ) {
      result = core::Cons_O::create(core::Pointer_O::create(findOwner._addresses[ii]),result);
    }
    return result;
  }
#else
  MISSING_GC_SUPPORT();
#endif // USE_BOEHM
  SIMPLE_ERROR(BF("You must pass a pointer"));
}

};


namespace gctools {
#ifdef USE_MPS
DOCGROUP(clasp)
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
  common_function_call_counter(HeaderPtrToGeneralPtr<core::General_O>(header),
                               size, hash_table_raw);
};
#endif

CL_LAMBDA(func)
CL_DECLARE();
CL_DOCSTRING(R"dx(function-call-count-profiler - Evaluate a function, count every function call made during the evaluation.)dx")
DOCGROUP(clasp)
CL_DEFUN void gctools__function_call_count_profiler(core::T_sp func) {
  core::HashTable_sp func_counters_start = core::HashTableEq_O::create_default();
  core::HashTable_sp func_counters_end = core::HashTableEq_O::create_default();
#if defined(USE_MPS)
  mps_amc_apply(global_amc_pool, amc_apply_function_call_counter, &*func_counters_start, 0);
#elif defined(USE_BOEHM)
# if GC_VERSION_MAJOR >= 7 && GC_VERSION_MINOR >= 6
  GC_enumerate_reachable_objects_inner(boehm_callback_function_call_counter, &*func_counters_start);
# endif
#elif defined(USE_MMTK)
  MISSING_GC_SUPPORT();
#endif
  core::eval::funcall(func);
#if defined(USE_MPS)
  mps_amc_apply(global_amc_pool, amc_apply_function_call_counter, &*func_counters_end, 0);
#elif defined(USE_BOEHM)
# if GC_VERSION_MAJOR >= 7 && GC_VERSION_MINOR >= 6
  GC_enumerate_reachable_objects_inner(boehm_callback_function_call_counter, &*func_counters_end);
# endif
#endif
  func_counters_start->mapHash([func_counters_end](core::T_sp f, core::T_sp start_value) {
      core::T_sp end_value = func_counters_end->gethash(f);
      ASSERT(start_value.fixnump() && end_value.fixnump());
      Fixnum diff = end_value.unsafe_fixnum() - start_value.unsafe_fixnum();
      func_counters_end->setf_gethash(f,core::clasp_make_fixnum(diff));
    } );

  core::List_sp results = nil<core::T_O>();
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
/*! Call finalizer_callback with no arguments when object is finalized.*/
DOCGROUP(clasp)
CL_DEFUN void gctools__finalize(core::T_sp object, core::T_sp finalizer_callback) {
  //printf("%s:%d making a finalizer for %p calling %p\n", __FILE__, __LINE__, (void*)object.tagged_(), (void*)finalizer_callback.tagged_());
  WITH_READ_WRITE_LOCK(globals_->_FinalizersMutex);
  core::WeakKeyHashTable_sp ht = _lisp->_Roots._Finalizers;
  core::List_sp orig_finalizers = ht->gethash(object,nil<core::T_O>());
  core::List_sp finalizers = core::Cons_O::create(finalizer_callback,orig_finalizers);
//  printf("%s:%d      Adding finalizer to list new length --> %lu   list head %p\n", __FILE__, __LINE__, core::cl__length(finalizers), (void*)finalizers.tagged_());
  ht->hash_table_setf_gethash(object,finalizers);
    // Register the finalizer with the GC
#if defined(USE_BOEHM)
  boehm_set_finalizer_list(object.tagged_(),finalizers.tagged_());
#elif defined(USE_MPS)
  if (object.generalp() || object.consp()) {
    my_mps_finalize(object.raw_());
  }
#elif defined(USE_MMTK)
  MISSING_GC_SUPPORT();
#endif
};

DOCGROUP(clasp)
CL_DEFUN void gctools__definalize(core::T_sp object) {
//  printf("%s:%d erasing finalizers for %p\n", __FILE__, __LINE__, (void*)object.tagged_());
  WITH_READ_WRITE_LOCK(globals_->_FinalizersMutex);
  core::WeakKeyHashTable_sp ht = _lisp->_Roots._Finalizers;
  if (ht->gethash(object)) ht->remhash(object);
#if defined(USE_BOEHM)
  boehm_clear_finalizer_list(object.tagged_());
#elif defined(USE_MPS)
  // Don't use mps_definalize here - definalize is taken care of by erasing the weak-key-hash-table
  // entry above.  We may still need to get a finalization message if this class needs
  // its destructor be called.
  MISSING_GC_SUPPORT();
#elif defined(USE_MMTK)
  MISSING_GC_SUPPORT();
#endif
}

};


namespace gctools {

};





namespace gctools {

DOCGROUP(clasp)
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

                
DOCGROUP(clasp)
CL_DEFUN core::T_sp gctools__stack_depth() {
  int z = 0;
  void *zp = &z;
  size_t stackDepth = (char *)_global_stack_marker - (char *)zp;
  return core::make_fixnum((uint)stackDepth);
};

DOCGROUP(clasp)
CL_DEFUN void gctools__garbage_collect() {
#if defined(USE_BOEHM)
  GC_gcollect();
//  write_bf_stream(BF("GC_invoke_finalizers\n"));
  GC_invoke_finalizers();
#elif defined(USE_MPS)
  mps_arena_collect(global_arena);
  size_t finalizations;
  processMpsMessages(finalizations);
  mps_arena_release(global_arena);
#elif defined(USE_MMTK)
  MISSING_GC_SUPPORT();
#endif
  //        printf("Garbage collection done\n");
};

DOCGROUP(clasp)
CL_DEFUN void gctools__register_stamp_name(const std::string& name,size_t stamp_num)
{
  register_stamp_name(name,stamp_num);
}

DOCGROUP(clasp)
CL_DEFUN core::T_sp gctools__get_stamp_name_map() {
  core::List_sp l = nil<core::T_O>();
  for ( auto it : global_unshifted_nowhere_stamp_name_map ) {
    l = core::Cons_O::create(core::Cons_O::create(core::SimpleBaseString_O::make(it.first),core::make_fixnum(it.second)),l);
  }
  return l;
}

CL_DOCSTRING(R"dx(Process finalizers)dx")
DOCGROUP(clasp)
CL_LAMBDA(&optional verbose)CL_DEFUN void gctools__cleanup(bool verbose) {
#ifdef USE_MPS
  size_t finalizations;
  size_t messages = processMpsMessages(finalizations);
  if (verbose) {
    core::write_bf_stream(BF("Processed %d finalization messages and %d total messages\n") % messages % finalizations );
  }
#endif
}

CL_DOCSTRING(R"dx(Set the number of signal polling ticks per GC cleanup and message processing.)dx")
CL_LAMBDA(&optional verbose)
DOCGROUP(clasp)
CL_DEFUN void gctools__poll_ticks_per_cleanup(int ticks) {
#ifdef USE_MPS
  global_pollTicksPerCleanup = ticks;
#endif
};



#define ARGS_gctools__debug_allocations "(arg)"
#define DECL_gctools__debug_allocations ""
#define DOCS_gctools__debug_allocations "debugAllocations"
DOCGROUP(clasp)
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

  bool debug_alloc_alignment = false;
#ifdef DEBUG_ALLOC_ALIGNMENT
  debug_alloc_alignment = true;
  debugging = true;
#endif
  if (buildReport) ss << (BF("DEBUG_ALLOC_ALIGNMENT = %s\n") % (debug_alloc_alignment ? "**DEFINED**" : "undefined") ).str();

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
  bool debug_mps_underscanning_initial = true;
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

  bool debug_guard_backtrace = false;
#ifdef DEBUG_GUARD_BACKTRACE
  debug_guard_backtrace = true;
  debugging = true;
  if (setFeatures)  features = core::Cons_O::create(_lisp->internKeyword("DEBUG-GUARD-BACKTRACE"), features);
#endif
  if (buildReport) ss << (BF("DEBUG_GUARD_BACKTRACE = %s\n") % (debug_guard_backtrace ? "**DEFINED**" : "undefined") ).str();
  
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

  bool sanitize_memory = false;
#ifdef SANITIZE_MEMORY
  sanitize_memory = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("SANITIZE-MEMORY"),features);
#endif
  if (buildReport) ss << (BF("SANITIZE_MEMORY = %s\n") % (sanitize_memory ? "**DEFINED**" : "undefined") ).str();


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

  bool debug_verify_modules = false;
#ifdef DEBUG_VERIFY_MODULES
  debug_verify_modules = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-VERIFY-MODULES"), features);
#endif
  if (buildReport) ss << (BF("DEBUG_VERIFY_MODULES = %s\n") % (debug_verify_modules ? "**DEFINED**" : "undefined")).str();

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

  bool debug_stores = false;
#ifdef DEBUG_STORES
  debug_stores = true;
  debugging = true;
  if (setFeatures) features = core::Cons_O::create(_lisp->internKeyword("DEBUG-STORES"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_STORES = %s\n") % (debug_stores ? "**DEFINED**" : "undefined") ).str();

  bool disable_type_inference = false;
#ifdef DISABLE_TYPE_INFERENCE
  disable_type_inference = true;
  debugging = true;
  if (setFeatures)  features = core::Cons_O::create(_lisp->internKeyword("DISABLE-TYPE-INFERENCE"),features);
#endif
  if (buildReport) ss << (BF("DISABLE_TYPE_INFERENCE = %s\n") % (disable_type_inference ? "**DEFINED**" : "undefined") ).str();

  bool use_compile_file_parallel = true;
#if USE_COMPILE_FILE_PARALLEL == 0
  use_compile_file_parallel = false;
  INTERN_(comp,STARuse_compile_file_parallelSTAR)->defparameter(nil<core::T_O>());
  printf("%s:%d You have turned off compile-file-parallel\n   - you can enable it by setting USE_COMPILE_FILE_PARALLEL in the wscript.config\n   - compile-file-parallel should be enabled by default\n", __FILE__, __LINE__ );
#else
  INTERN_(comp,STARuse_compile_file_parallelSTAR)->defparameter(_lisp->_true());
#endif
  if (buildReport) ss << (BF("USE_COMPILE_FILE_PARALLEL = %s\n") % USE_COMPILE_FILE_PARALLEL);

  bool force_startup_external_linkage = true;
#if FORCE_STARTUP_EXTERNAL_LINKAGE == 0
  force_startup_external_linkage = false;
  INTERN_(comp,STARforce_startup_external_linkageSTAR)->defparameter(nil<core::T_O>());
#else
  INTERN_(comp,STARforce_startup_external_linkageSTAR)->defparameter(_lisp->_true());
#endif
  if (buildReport) ss << (BF("FORCE_STARTUP_EXTERNAL_LINKAGE = %s\n") % FORCE_STARTUP_EXTERNAL_LINKAGE);
  
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
#elif CLASP_BUILD_MODE == 3
  use_lto = false;
  debugging = false;
  INTERN_(core,STARclasp_build_modeSTAR)->defparameter(kw::_sym_faso);
#elif CLASP_BUILD_MODE == 4
  use_lto = false;
  debugging = false;
  INTERN_(core,STARclasp_build_modeSTAR)->defparameter(kw::_sym_fasoll);
#elif CLASP_BUILD_MODE == 5
  use_lto = false;
  debugging = false;
  INTERN_(core,STARclasp_build_modeSTAR)->defparameter(kw::_sym_fasobc);
#endif
  if (buildReport) ss << (BF("CLASP_BUILD_MODE = %s\n") % CLASP_BUILD_MODE);
  
  bool use_human_readable_bitcode = false;
#if USE_HUMAN_READABLE_BITCODE==1
  use_human_readable_bitcode = true;
  debugging = true;
  if (setFeatures)  features = core::Cons_O::create(_lisp->internKeyword("USE-HUMAN-READABLE-BITCODE"),features);
#endif
  if (buildReport) ss << (BF("USE_HUMAN_READABLE_BITCODE = %s\n") % (use_human_readable_bitcode ? "**DEFINED**" : "undefined") ).str();

  bool debug_compile_file_output_info = false;
#if DEBUG_COMPILE_FILE_OUTPUT_INFO==1
  debug_compile_file_output_info = true;
  debugging = true;
  if (setFeatures)  features = core::Cons_O::create(_lisp->internKeyword("DEBUG-COMPILE-FILE-OUTPUT-INFO"),features);
#endif
  if (buildReport) ss << (BF("DEBUG_COMPILE_FILE_OUTPUT_INFO = %s\n") % (debug_compile_file_output_info ? "**DEFINED**" : "undefined") ).str();

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

DOCGROUP(clasp)
CL_DEFUN void gctools__configuration()
{
  stringstream ss;
  bool debugging = debugging_configuration(false,true,ss);
  core::clasp_writeln_string(ss.str());
}

DOCGROUP(clasp)
CL_DEFUN void gctools__thread_local_cleanup()
{
#ifdef DEBUG_FINALIZERS
  printf("%s:%d:%s  called to cleanup thread local resources\n", __FILE__, __LINE__, __FUNCTION__ );
#endif
  core::thread_local_invoke_and_clear_cleanup();
}

DOCGROUP(clasp)
CL_DEFUN core::Integer_sp gctools__unwind_time_nanoseconds() {
  core::Integer_sp is = core::Integer_O::create(my_thread_low_level->_unwind_time.count());
  return is;
}

void initialize_gc_functions() {
  _sym_STARallocPatternStackSTAR->defparameter(nil<core::T_O>());
#ifdef USE_MPS
//  core::af_def(GcToolsPkg, "mpsTelemetrySet", &gctools__mpsTelemetrySet);
//  core::af_def(GcToolsPkg, "mpsTelemetryReset", &gctools__mpsTelemetryReset);
//  core::af_def(GcToolsPkg, "mpsTelemetryFlush", &gctools__mpsTelemetryFlush);
#endif
};
};
