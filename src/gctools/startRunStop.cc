/*
    File: memoryManagement.cc
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
//

#define DEBUG_LEVEL_NONE

#include <unistd.h>
#include <fcntl.h>
#include <libgen.h>
#include <clasp/core/foundation.h>
#include <clasp/gctools/gcalloc.h>
#include <clasp/core/object.h>
#include <clasp/core/bformat.h>
#include <clasp/core/numbers.h>
#include <clasp/core/array.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/debugger.h>
#include <clasp/core/evaluator.h>
#include <clasp/gctools/gc_boot.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/gctools/snapshotSaveLoad.h>
#include <clasp/gctools/memoryManagement.h>
#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/clbind/clbindPackage.h>
#include <clasp/sockets/socketsPackage.h>
#include <clasp/serveEvent/serveEventPackage.h>
#include <clasp/asttooling/asttoolingPackage.h>
#include <clasp/gctools/snapshotSaveLoad.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/bundle.h>
#include <clasp/core/lisp.h>
#include <clasp/core/posixTime.h>
#include <clasp/core/compiler.h>
#include <clasp/core/primitives.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/code.h>
#include <clasp/clbind/open.h>
#include <clasp/gctools/gc_interface.fwd.h>
#include <clasp/mpip/claspMpi.h>
// #include "main/allHeaders.cc"

#ifdef _TARGET_OS_DARWIN
#include <mach-o/getsect.h>
#endif

#ifndef SCRAPING
#define ALL_EXPOSES_EXTERN
#include EXPOSE_INC_H
#undef ALL_EXPOSES_EXTERN
#endif

// ---------------------------------------------------------------------------
// GLOBAL VARS
// ---------------------------------------------------------------------------

#define CLASP_DEFAULT_PROGRAM_NAME "clasp"
#define CLASP_DEFAULT_EXE_NAME CLASP_DEFAULT_PROGRAM_NAME

static std::string g_exe_name;     // filename of the executable
static std::string g_program_name; // logical / settable program name

static bool g_abort_flag;

static std::terminate_handler g_prev_terminate_handler;

namespace gctools {

size_t global_alignup_sizeof_header;

void monitorAllocation(stamp_t k, size_t sz) {
#ifdef DEBUG_MONITOR_ALLOCATIONS
  if (global_monitorAllocations.counter >= global_monitorAllocations.start &&
      global_monitorAllocations.counter < global_monitorAllocations.end) {
    core::core__clib_backtrace(global_monitorAllocations.backtraceDepth);
  }
  global_monitorAllocations.counter++;
#endif
}

int handleFatalCondition() {
  int exitCode = 0;
  try {
    throw;
  } catch (core::CatchThrow& ee) {
    core::clasp_write_string(fmt::format("{}:{} Uncaught THROW tag[{}] - this should NEVER happen - the stack should never be "
                                         "unwound unless there is a CATCH clause that matches the THROW",
                                         __FILE__, __LINE__, ee.getTag()));
  } catch (core::Unwind& ee) {
    core::clasp_write_string(
        fmt::format("At {}:{} - Unwind caught frame: {} index: {}", __FILE__, __LINE__, (void*)ee.getFrame(), ee.index()));
  } catch (HardError& ee) {
    core::clasp_write_string(fmt::format("At {}:{} - HardError caught: {}", __FILE__, __LINE__, ee.message()));
  }
  return exitCode;
}

void set_abort_flag(bool abort_flag = false) { g_abort_flag = abort_flag; }

bool abort_flag(void) { return g_abort_flag; }

// EXECUTABLE MASTER DATA
// - PROGRAM NAME
void set_program_name(std::string program_name = CLASP_DEFAULT_PROGRAM_NAME) { g_program_name = program_name; }

std::string program_name() { return g_program_name; }

// - EXECUTABLE NAME
void set_exe_name(std::string exe_name = CLASP_DEFAULT_EXE_NAME) { g_exe_name = exe_name; }

std::string exe_name() { return g_exe_name; }

// TERMINATION HANDLING

static void clasp_terminate_handler(void) {
  // TODO: Implement CLASP terminate handler, e.g.:
  // - Call all functions registered via an atexit hook -
  // to be implemented!

  // Finally exit or abort

  if (gctools::abort_flag())
    abort();
  try {
    throw;
  } catch (const std::exception& e) {
    fprintf(stderr, "%s:%d There was an unhandled std::exception in process [pid: %d] e.what()=[%s] - do something about it.\n",
            __FILE__, __LINE__, getpid(), e.what());
  } catch (...) {
    fprintf(stderr, "%s:%d There was an unhandled unknown exception in process [pid: %d] - do something about it.\n", __FILE__,
            __LINE__, getpid());
  };
  abort();
}

CL_DEFUN void gctools__register_loaded_objects() {
  core::add_library addlib;
  core::startup_register_loaded_objects(&addlib);
}

}; // namespace gctools

#ifndef SCRAPING
#define ALL_PREGCSTARTUPS_EXTERN
#include PRE_GC_STARTUP_INC_H
#undef ALL_PREGCSTARTUPS_EXTERN
#endif

extern "C" {

int startup_clasp(void** stackMarker, gctools::ClaspInfo* claspInfo, int* exitCode) {
  gctools::_global_stack_max_size = claspInfo->_stackMax;
  gctools::global_alignup_sizeof_header = gctools::AlignUp(sizeof(gctools::Header_s));
  gctools::global_sizeof_fwd = gctools::AlignUp(sizeof(gctools::Header_s));

  const char* trigger = getenv("CLASP_DISCRIMINATING_FUNCTION_TRIGGER");
  if (trigger) {
    size_t strigger = atoi(trigger);
    core::global_compile_discriminating_function_trigger = strigger;
    printf("%s:%d:%s Setting global_compile_discriminating_function_trigger = %lu\n", __FILE__, __LINE__, __FUNCTION__, strigger);
  }
  if (getenv("CLASP_TIME_EXIT")) {
    atexit(core::last_exit);
  }
  const char* dof = getenv("CLASP_DEBUG_OBJECT_FILES");
  if (dof) {
    if (strcmp(dof, "save") == 0) {
      llvmo::globalDebugObjectFiles = llvmo::DebugObjectFilesPrintSave;
    } else {
      llvmo::globalDebugObjectFiles = llvmo::DebugObjectFilesPrint;
    }
  }

#ifdef DEBUG_DYN_ENV_STACK
  const char* ddes = getenv("CLASP_DEBUG_DYN_ENV_STACK");
  if (ddes)
    core::global_debug_dyn_env_stack = true;
#endif

  // DO BASIC EXE SETUP

  gctools::set_abort_flag(); // Set abort flag to default value
  g_prev_terminate_handler = std::set_terminate([]() { gctools::clasp_terminate_handler(); });

  // - STORE NAME OF EXECUTABLE

  {
    std::string exename(claspInfo->_argv[0]);
    gctools::set_exe_name(basename((char*)exename.c_str()));
  }

  // - SET THE APPLICATION NAME

  gctools::set_program_name();

  fflush(stderr);

  //
  // Setup debugging info all the time
  //
  core::dumpDebuggingLayouts();
  if (getenv("CLASP_DEBUGGER_SUPPORT")) {
    stringstream ss;
    char* username = getenv("USER");
    if (!username) {
      printf("%s:%d:%s Could not get USER environment variable\n", __FILE__, __LINE__, __FUNCTION__);
      exit(1);
    }
    ss << "/tmp/clasp_pid_" << getenv("USER");
    printf("%s:%d:%s  Setting up clasp for debugging - writing PID to %s\n", __FILE__, __LINE__, __FUNCTION__, ss.str().c_str());
    FILE* fout = fopen(ss.str().c_str(), "w");
    if (!fout) {
      printf("%s:%d:%s Could not open %s\n", __FILE__, __LINE__, __FUNCTION__, ss.str().c_str());
      exit(1);
    }
    fprintf(fout, "%d", getpid());
    fclose(fout);
  }
  //
  // Pause before any allocations take place
  //
  char* pause_startup = getenv("CLASP_PAUSE_STARTUP");
  if (pause_startup) {
    gctools::setup_user_signal();
    gctools::wait_for_user_signal("Paused at startup before all initialization");
  }

  //
  // Walk the stamp field layout tables.
  //
  stringstream ssdummy;
  walk_stamp_field_layout_tables(gctools::precise_info, ssdummy);

  gctools::initialize_signals();

  core::global_options = new core::CommandLineOptions(claspInfo->_argc, claspInfo->_argv);
  
#ifndef SCRAPING
#define ALL_PREGCSTARTUPS_CALLS
#include PRE_GC_STARTUP_INC_H
#undef ALL_PREGCSTARTUPS_CALLS
#endif
    
#if defined(USE_MPS)
  gctools::startupMemoryPoolSystem(claspInfo);
#elif defined(USE_BOEHM)
  gctools::startupBoehm(claspInfo); // Correct
#elif defined(USE_MMTK)
  int exitCode = gctools::initializeMmtk(startupFn, claspInfo->_argc, claspInfo->_argv, claspInfo->_mpiEnabled, claspInfo->_mpiRank,
                                         claspInfo->_mpiSize);
#endif

  core::transfer_StartupInfo_to_my_thread();
  my_thread->startUpVM();

  // Register builtin function names
  define_builtin_cxx_class_names();

  // Read the memory profiling settings <size-threshold> <number-theshold>
  // as in export CLASP_MEMORY_PROFILE="16000000 1024"
  // This means call HitAllocationSizeThreshold every time 16000000 bytes are allocated
  //        and call HitAllocationNumberThreshold every time 1024 allocations take place
  char* cur = getenv("CLASP_MEMORY_PROFILE");
  size_t values[2];
  int numValues = 0;
  if (cur) {
    while (*cur && numValues < 2) {
      values[numValues] = strtol(cur, &cur, 10);
      ++numValues;
    }
    if (numValues == 2) {
      my_thread_low_level->_Allocations._AllocationNumberThreshold = values[1];
    }
    if (numValues >= 1) {
      my_thread_low_level->_Allocations._AllocationSizeThreshold = values[0];
    }
  }

  //
  // Walk all of the loaded dynamic libraries
  //
  core::add_library addlib;
  startup_register_loaded_objects(&addlib);
  {
    core::global_initialize_builtin_classes = true;
    initialize_clasp_Kinds();
    core::global_initialize_builtin_classes = false;
  }

  void* start_of_snapshot = NULL;
  void* end_of_snapshot = NULL;

#ifdef USE_PRECISE_GC
#ifdef _TARGET_OS_DARWIN
  const struct mach_header_64* exec_header = (const struct mach_header_64*)dlsym(RTLD_DEFAULT, "_mh_execute_header");
  size_t size;
  start_of_snapshot = getsectiondata(exec_header, SNAPSHOT_SEGMENT, SNAPSHOT_SECTION, &size);
  end_of_snapshot = NULL;
  if (start_of_snapshot) {
    end_of_snapshot = (void*)((char*)start_of_snapshot + size);
  }
#endif
#if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_FREEBSD)
  extern const char __attribute__((weak)) SNAPSHOT_START;
  extern const char __attribute__((weak)) SNAPSHOT_END;
  start_of_snapshot = (void*)&SNAPSHOT_START;
  end_of_snapshot = (void*)&SNAPSHOT_END;
#endif
#endif

  if (start_of_snapshot) {
    core::global_options->_StartupType = core::cloEmbeddedSnapshot;
    core::global_options->_FreezeStartupType = true;
  }

  // Do some minimal argument processing
  (core::global_options->_ProcessArguments)(core::global_options);
  ::globals_ = new core::globals_t();
  globals_->_DebugStream = new core::DebugStream(claspInfo->_mpiRank);

  // - MPI ENABLEMENT

#ifdef USE_MPI
  if (!core::global_options->_DisableMpi) {
    if (!core::global_options->_NoInform) {
      fmt::print("Enabling MPI...\n");
    }
    try {
      int argc = claspInfo->_argc;
      char** argv = (char**)claspInfo->_argv;
      mpip::Mpi_O::Init(argc, argv, claspInfo->_mpiEnabled, claspInfo->_mpiRank, claspInfo->_mpiSize);
    } catch (HardError& err) {
      fprintf(stderr, "**** %s (%s:%d): ERROR: Could not start MPI - ABORTING!\n", gctools::exe_name().c_str(), __FILE__, __LINE__);
      abort();
    }
  } else {
    claspInfo->_mpiEnabled = false;
  }
#endif

  const char* jls = getenv("CLASP_JIT_LOG_SYMBOLS");
  if (jls || core::global_options->_JITLogSymbols) {
    core::global_jit_log_symbols = true;
  }

  //
  // Look around the local directories for source and fasl files.
  //
  core::Bundle* bundle = new core::Bundle(core::global_options->_ExecutableName);
  globals_->_Bundle = bundle;

  //
  // Figure out if we are starting up with a snapshot or an image
  //  Set loadSnapshotFile = true if we load a snapshot
  //

  if (core::global_options->_StartupType == core::cloSnapshotFile ||
      core::global_options->_StartupType == core::cloEmbeddedSnapshot) {
    //
    // Load a snapshot from a file (loadSnapshotFile=true) or from memory (start_of_snapshot!=NULL)
    //
#ifdef USE_PRECISE_GC
    if (core::global_options->_StartupType == core::cloSnapshotFile &&
        core::startup_snapshot_is_stale(core::global_options->_StartupFile)) {
      fmt::print("The startup snapshot file \"{}\" is stale - remove it or create a new one\n", core::global_options->_StartupFile);
      std::exit(1);
    }
    llvmo::initialize_llvm();

    clbind::initializeCastGraph();
    snapshotSaveLoad::snapshot_load(start_of_snapshot, end_of_snapshot, core::global_options->_StartupFile);
    _lisp->parseCommandLineArguments(*core::global_options);
#endif
  } else {
    //
    // Startup clasp using an image and running all toplevel forms
    //
    claspInfo->_lispHolder = new core::LispHolder(claspInfo->_mpiEnabled, claspInfo->_mpiRank, claspInfo->_mpiSize);
    gctools::GcToolsExposer_O GcToolsPkg(_lisp);
    clbind::ClbindExposer_O ClbindPkg(_lisp);
    llvmo::LlvmoExposer_O llvmopkg(_lisp);
    sockets::SocketsExposer_O SocketsPkg(_lisp);
    serveEvent::ServeEventExposer_O ServeEventPkg(_lisp);
    asttooling::AsttoolingExposer_O AsttoolingPkg(_lisp);

    claspInfo->_lispHolder->startup(*core::global_options);

    _lisp->installPackage(&GcToolsPkg);
    _lisp->installPackage(&ClbindPkg);
    _lisp->installPackage(&llvmopkg);
    _lisp->installPackage(&SocketsPkg);
    _lisp->installPackage(&ServeEventPkg);
    _lisp->installPackage(&AsttoolingPkg);

#ifndef SCRAPING
#define ALL_EXPOSES_CALLS
#include EXPOSE_INC_H
#undef ALL_EXPOSES_CALLS
#endif

    core::_sym_STARmpi_rankSTAR->defparameter(core::make_fixnum(0));
    core::_sym_STARmpi_sizeSTAR->defparameter(core::make_fixnum(1));

#ifdef USE_MPI
    mpip::MpiExposer_O TheMpiPkg(_lisp);
    _lisp->installPackage(&TheMpiPkg);
    if (claspInfo->_mpiEnabled) {
      core::Symbol_sp mpi = _lisp->internKeyword("MPI-ENABLED");
      core::Cons_sp features = cl::_sym_STARfeaturesSTAR->symbolValue().as<core::Cons_O>();
      cl::_sym_STARfeaturesSTAR->defparameter(core::Cons_O::create(mpi, features));
      core::_sym_STARmpi_rankSTAR->defparameter(core::make_fixnum(claspInfo->_mpiRank));
      core::_sym_STARmpi_sizeSTAR->defparameter(core::make_fixnum(claspInfo->_mpiSize));
    }
#endif
  }

  //
  // If --addresses was passed as a command line option - dump the addresses here
  //
  maybeHandleAddressesOption(core::global_options);

  if (core::initializer_functions_are_waiting()) {
    core::initializer_functions_invoke();
  }

#ifdef DEBUG_PROGRESS
  printf("%s:%d run\n", __FILE__, __LINE__);
#endif

  // If the user adds "-f debug-startup" to the command line
  // then set core::*debug-startup* to true
  // This will print timings of top-level forms as they load at startup
  // See llvmo::intrinsics.cc
  // cl__member isn't available yet so check for the feature by hand.
  for (auto cur : (core::List_sp)cl::_sym_STARfeaturesSTAR->symbolValue()) {
    if (oCar(cur) == kw::_sym_debugStartup) {
      printf("%s:%d Setting core:*debug-startup* to T\n", __FILE__, __LINE__);
      core::_sym_STARdebugStartupSTAR->setf_symbolValue(_lisp->_true());
    } else if (oCar(cur) == kw::_sym_debugStartupVerbose) {
      printf("%s:%d Setting core:*debug-startup* to :verbose\n", __FILE__, __LINE__);
      core::_sym_STARdebugStartupSTAR->setf_symbolValue(kw::_sym_verbose);
    } else if (oCar(cur) == kw::_sym_exit_backtrace) {
      printf("%s:%d Setting core:*exit-backtrace* to T\n", __FILE__, __LINE__);
      core::_sym_STARexit_backtraceSTAR->setf_symbolValue(_lisp->_true());
    } else if (oCar(cur) == kw::_sym_pause_pid) {
      gctools::wait_for_user_signal("Paused at startup");
    }
  }
  // The system is fully up now
  _lisp->_Roots._TheSystemIsUp = true;
  core::Package_sp cluser = gc::As<core::Package_sp>(_lisp->findPackage("COMMON-LISP-USER"));
  cl::_sym_STARpackageSTAR->defparameter(cluser);

  return _lisp->load(*exitCode);
}

int run_clasp(gctools::ClaspInfo* claspInfo) {
  int exitCode;
  try {
    exitCode = _lisp->run();
  } catch (snapshotSaveLoad::SaveLispAndDie& ee) {
#ifdef USE_PRECISE_GC
    snapshotSaveLoad::snapshot_save(ee);
#endif
    exitCode = 0;
  }
  return exitCode;
};

void shutdown_clasp(gctools::ClaspInfo* claspInfo) {
  delete claspInfo->_lispHolder;
  _lisp->_Roots._ClaspJIT = nil<core::T_O>();
  mp::ClaspThreads_exit(); // run pthreads_exit
#ifdef USE_MPI
  if (claspInfo->_mpiEnabled) {
    mpip::Mpi_O::Finalize();
  }
#endif
};
};
