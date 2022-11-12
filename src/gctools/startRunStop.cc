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
//#include "main/allHeaders.cc"

// ---------------------------------------------------------------------------
// GLOBAL VARS
// ---------------------------------------------------------------------------

#define CLASP_DEFAULT_PROGRAM_NAME "clasp"
#define CLASP_DEFAULT_EXE_NAME CLASP_DEFAULT_PROGRAM_NAME

static std::string g_exe_name;      // filename of the executable
static std::string g_program_name;  // logical / settable program name

static bool        g_abort_flag;

static std::terminate_handler g_prev_terminate_handler;




namespace gctools {

size_t global_alignup_sizeof_header;

void monitorAllocation(stamp_t k, size_t sz) {
#ifdef DEBUG_MONITOR_ALLOCATIONS  
  if (global_monitorAllocations.counter >= global_monitorAllocations.start && global_monitorAllocations.counter < global_monitorAllocations.end) {
    core::core__clib_backtrace(global_monitorAllocations.backtraceDepth);
  }
  global_monitorAllocations.counter++;
#endif
}


int handleFatalCondition() {
  int exitCode = 0;
  try {
    throw;
  } catch (core::ExitProgramException &ee) {
    // Do nothing
    //            printf("Caught ExitProgram in %s:%d\n", __FILE__, __LINE__);
    exitCode = ee.getExitResult();
  } catch (core::TerminateProgramIfBatch &ee) {
    // Do nothing
    printf("Caught TerminateProgramIfBatch in %s:%d\n", __FILE__, __LINE__);
  } catch (core::CatchThrow &ee) {
    core::write_bf_stream(fmt::sprintf("%s:%d Uncaught THROW tag[%s] - this should NEVER happen - the stack should never be unwound unless there is a CATCH clause that matches the THROW", __FILE__ , __LINE__ , ee.getTag()));
  } catch (core::Unwind &ee) {
    core::write_bf_stream(fmt::sprintf("At %s:%d - Unwind caught frame: %p index: %d", __FILE__ , __LINE__ , (void*)ee.getFrame() , ee.index()));
  } catch (HardError &ee) {
    core::write_bf_stream(fmt::sprintf("At %s:%d - HardError caught: %s", __FILE__ , __LINE__ , ee.message()));
  }
  return exitCode;
}


void set_abort_flag( bool abort_flag = false )
{
  g_abort_flag = abort_flag;
}

bool abort_flag( void )
{
  return g_abort_flag;
}

// EXECUTABLE MASTER DATA
// - PROGRAM NAME
void set_program_name( std::string program_name = CLASP_DEFAULT_PROGRAM_NAME )
{
  g_program_name = program_name;
}

std::string program_name()
{
  return g_program_name;
}

// - EXECUTABLE NAME
void set_exe_name( std::string exe_name = CLASP_DEFAULT_EXE_NAME )
{
  g_exe_name = exe_name;
}

std::string exe_name()
{
  return g_exe_name;
}

// TERMINATION HANDLING

static void clasp_terminate_handler( void )
{
  // TODO: Implement CLASP terminate handler, e.g.:
  // - Call all functions registered via an atexit hook -
  // to be implemented!

  // Finally exit or abort

  if( gctools::abort_flag() )
    abort();
  try { throw; }
  catch (const std::exception& e) {
      fprintf(stderr, "%s:%d There was an unhandled std::exception in process [pid: %d] e.what()=[%s] - do something about it.\n", __FILE__, __LINE__, getpid(), e.what()  );
  } catch (...) {
      fprintf(stderr, "%s:%d There was an unhandled unknown exception in process [pid: %d] - do something about it.\n", __FILE__, __LINE__, getpid() );
  };
  abort();
}

};


extern "C" {

void startup_clasp( void** stackMarker, gctools::ClaspInfo* claspInfo ) {

  if (gctools::Header_s::weak_mtag != gctools::character_tag) {
    printf("%s:%d:%s The Header_s::weak_mtag (%lu) MUST have the same value as gctools::character_tag(%lu)\n",
           __FILE__, __LINE__, __FUNCTION__, (uintptr_t)gctools::Header_s::weak_mtag, (uintptr_t)gctools::character_tag);
    abort();
  }
  gctools::_global_stack_marker = (const char*)stackMarker;
  gctools::_global_stack_max_size = claspInfo->_stackMax;
  gctools::global_alignup_sizeof_header = gctools::AlignUp(sizeof(gctools::Header_s));
  gctools::global_sizeof_fwd = gctools::AlignUp(sizeof(gctools::Header_s));

  const char* trigger = getenv("CLASP_DISCRIMINATING_FUNCTION_TRIGGER");
  if (trigger) {
    size_t strigger = atoi(trigger);
    core::global_compile_discriminating_function_trigger = strigger;
    printf("%s:%d:%s Setting global_compile_discriminating_function_trigger = %lu\n", __FILE__, __LINE__, __FUNCTION__, strigger );
  }
  if (getenv("CLASP_TIME_EXIT")) {
    atexit(core::last_exit);
  }
  const char* dof = getenv("CLASP_DEBUG_OBJECT_FILES");
  if (dof) {
    if (strcmp(dof,"save")==0) {
      llvmo::globalDebugObjectFiles = llvmo::DebugObjectFilesPrintSave;
    } else {
      llvmo::globalDebugObjectFiles = llvmo::DebugObjectFilesPrint;
    }
  }

#ifdef DEBUG_DYN_ENV_STACK
  const char* ddes = getenv("CLASP_DEBUG_DYN_ENV_STACK");
  if (ddes) core::global_debug_dyn_env_stack = true;
#endif
  

  // Do not touch debug log until after MPI init

  bool mpiEnabled = false;
  int  mpiRank    = 0;
  int  mpiSize    = 1;


  // DO BASIC EXE SETUP

  gctools::set_abort_flag(); // Set abort flag to default value
  g_prev_terminate_handler = std::set_terminate( [](){ gctools::clasp_terminate_handler(); } );

  // - STORE NAME OF EXECUTABLE

  {
    std::string exename( claspInfo->_argv[ 0 ] );
    gctools::set_exe_name( basename( (char *) exename.c_str() ) );
  }

  // - SET THE APPLICATION NAME

  gctools::set_program_name();

  
  // - COMMAND LINE OPTONS HANDLING

  core::CommandLineOptions options(claspInfo->_argc, claspInfo->_argv);

  // - MPI ENABLEMENT

#ifdef USE_MPI
  if (!options._DisableMpi) {
    printf("%s:%d Enabling MPI\n", __FILE__, __LINE__ );
    try
    {
      mpip::Mpi_O::Init(argc, argv, mpiEnabled, mpiRank, mpiSize);
    }
    catch ( HardError &err )
    {
      fprintf( stderr, "**** %s (%s:%d): ERROR: Could not start MPI - ABORTING!\n",
               exe_name().c_str(), __FILE__, __LINE__ );
      abort();
    }
  } else {
    mpiEnabled = false;
  }
#endif

  fflush( stderr );

  //
  // Setup debugging info all the time
  //
  core::dumpDebuggingLayouts();
  if (getenv("CLASP_DEBUGGER_SUPPORT")) {
    stringstream ss;
    char* username = getenv("USER");
    if (!username) {
      printf("%s:%d:%s Could not get USER environment variable\n", __FILE__, __LINE__, __FUNCTION__ );
      exit(1);
    }
    ss << "/tmp/clasp_pid_" << getenv("USER");
    printf("%s:%d:%s  Setting up clasp for debugging - writing PID to %s\n", __FILE__, __LINE__, __FUNCTION__, ss.str().c_str());
    FILE* fout = fopen(ss.str().c_str(),"w");
    if (!fout) {
      printf("%s:%d:%s Could not open %s\n", __FILE__, __LINE__, __FUNCTION__, ss.str().c_str() );
      exit(1);
    }
    fprintf(fout,"%d",getpid());
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
  walk_stamp_field_layout_tables(gctools::precise_info,ssdummy);
#ifdef SIGRTMIN
# define DEFAULT_THREAD_INTERRUPT_SIGNAL SIGRTMIN + 2
#else
# define DEFAULT_THREAD_INTERRUPT_SIGNAL SIGUSR1
#endif
  gctools::initialize_signals(DEFAULT_THREAD_INTERRUPT_SIGNAL);

#if defined(USE_MPS)
  gctools::startupMemoryPoolSystem(claspInfo);
#elif defined(USE_BOEHM)
  gctools::startupBoehm(claspInfo); // Correct
#elif defined(USE_MMTK)
  int exitCode = gctools::initializeMmtk(startupFn, argc, argv, mpiEnabled, mpiRank, mpiSize );
#endif

  // Register builtin function names
  define_builtin_cxx_class_names();
  
  // Read the memory profiling settings <size-threshold> <number-theshold>
  // as in export CLASP_MEMORY_PROFILE="16000000 1024"
  // This means call HitAllocationSizeThreshold every time 16000000 bytes are allocated
  //        and call HitAllocationNumberThreshold every time 1024 allocations take place
  char *cur = getenv("CLASP_MEMORY_PROFILE");
  size_t values[2];
  int numValues = 0;
  int exit_code = 0;
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
  
  // Do some minimal argument processing
  (core::global_options->_ProcessArguments)(core::global_options);
  ::globals_ = new core::globals_t();
  globals_->_DebugStream = new core::DebugStream(claspInfo->_mpiRank);

  const char* jls = getenv("CLASP_JIT_LOG_SYMBOLS");
  if (jls || core::global_options->_JITLogSymbols) {
    core::global_jit_log_symbols = true;
  }
  
//  printf("%s:%d:%s About to get start_of_snapshot\n", __FILE__, __LINE__, __FUNCTION__ );
#ifdef USE_PRECISE_GC
#  ifdef _TARGET_OS_DARWIN
  const struct mach_header_64 * exec_header = (const struct mach_header_64 *)dlsym(RTLD_DEFAULT,"_mh_execute_header");
  size_t size;
  claspInfo->_start_of_snapshot = getsectiondata(exec_header,
                                                 SNAPSHOT_SEGMENT,
                                                 SNAPSHOT_SECTION,
                                                 &size);
  void* claspInfo->_end_of_snapshot = NULL;
  if (claspInfo->_start_of_snapshot) {
    claspInfo->_end_of_snapshot = (void*)((char*)claspInfo->_start_of_snapshot + size);
  }
#  endif
#  ifdef _TARGET_OS_LINUX
  extern const char __attribute__((weak)) SNAPSHOT_START;
  extern const char __attribute__((weak)) SNAPSHOT_END;
  claspInfo->_start_of_snapshot = (void*)&SNAPSHOT_START;
  claspInfo->_end_of_snapshot = (void*)&SNAPSHOT_END;
#  endif
#else
  claspInfo->_start_of_snapshot = NULL;
  claspInfo->_end_of_snapshot = NULL;
#endif
  //
  // Look around the local directories for source and fasl files.
  //
  core::Bundle *bundle = new core::Bundle(core::global_options->_ExecutableName);
  globals_->_Bundle = bundle;

  //
  // Figure out if we are starting up with a snapshot or an image
  //  Set loadSnapshotFile = true if we load a snapshot
  //

#ifdef USE_PRECISE_GC  
  claspInfo->_snapshotFileName = "";
  if (!claspInfo->_start_of_snapshot) {
    if (core::global_options->_StartupFileP &&
        core::global_options->_StartupFileType == core::cloSnapshot) {
      claspInfo->_snapshotFileName = core::global_options->_StartupFile;
      claspInfo->_loadSnapshotFile = true;
    } else if (core::global_options->_DefaultStartupType == core::cloDefault) {
      claspInfo->_snapshotFileName = core::startup_snapshot_name(*bundle);
      if (std::filesystem::exists(std::filesystem::path(claspInfo->_snapshotFileName))) {
        claspInfo->_loadSnapshotFile = true;
      }
    } else if (core::global_options->_DefaultStartupType == core::cloSnapshot) {
      claspInfo->_snapshotFileName = core::startup_snapshot_name(*bundle);
      if (std::filesystem::exists(std::filesystem::path(claspInfo->_snapshotFileName))) {
        claspInfo->_loadSnapshotFile = true;
      } else {
        printf("Could not find snapshot file %s - exiting.\n", claspInfo->_snapshotFileName.c_str());
        exit(1);
      }
    }
  }
#endif

  if ( claspInfo->_loadSnapshotFile ||          // We want to load a snapshot
       (claspInfo->_start_of_snapshot != NULL) // We found an embedded snapshot
       ) {
    //
    // Load a snapshot from a file (loadSnapshotFile=true) or from memory (start_of_snapshot!=NULL)
    //
#ifdef USE_PRECISE_GC
    if (claspInfo->_loadSnapshotFile && core::startup_snapshot_is_stale(claspInfo->_snapshotFileName)) {
      printf("The startup snapshot file \"%s\" is stale - remove it or create a new one\n", claspInfo->_snapshotFileName.c_str() );
      std::exit(1);
    }
    llvmo::initialize_llvm();

    clbind::initializeCastGraph();
    if (claspInfo->_start_of_snapshot) {
      core::global_startupSourceName = "memory";
      core::global_startupEnum = core::snapshotMemory;
    } else {
      core::global_startupSourceName = claspInfo->_snapshotFileName;
      core::global_startupEnum = core::snapshotFile;
    }
    snapshotSaveLoad::snapshot_load( (void*)claspInfo->_start_of_snapshot, (void*)claspInfo->_end_of_snapshot, claspInfo->_snapshotFileName );

  } else {
    //
    // Startup clasp using an image and running all toplevel forms
    //
    core::LispHolder lispHolder(claspInfo->_mpiEnabled, claspInfo->_mpiRank, claspInfo->_mpiSize);
    gctools::GcToolsExposer_O GcToolsPkg(_lisp);
    clbind::ClbindExposer_O ClbindPkg(_lisp);
    llvmo::LlvmoExposer_O llvmopkg(_lisp);
    sockets::SocketsExposer_O SocketsPkg(_lisp);
    serveEvent::ServeEventExposer_O ServeEventPkg(_lisp);
    asttooling::AsttoolingExposer_O AsttoolingPkg(_lisp);

    lispHolder.startup(*core::global_options);

    _lisp->installPackage(&GcToolsPkg);
    _lisp->installPackage(&ClbindPkg);
    _lisp->installPackage(&llvmopkg);
    _lisp->installPackage(&SocketsPkg);
    _lisp->installPackage(&ServeEventPkg);
    _lisp->installPackage(&AsttoolingPkg);

#ifndef SCRAPING
# define ALL_EXPOSES_CALLS
# include EXPOSE_INC_H
# undef ALL_EXPOSES_CALLS
#endif

    core::_sym_STARmpi_rankSTAR->defparameter(core::make_fixnum(0));
    core::_sym_STARmpi_sizeSTAR->defparameter(core::make_fixnum(1));

#ifdef USE_MPI
    mpip::MpiExposer_O TheMpiPkg(_lisp);
    _lisp->installPackage(&TheMpiPkg);
    if (mpiEnabled) {
      core::Symbol_sp mpi = _lisp->internKeyword("MPI-ENABLED");
      core::Cons_sp features = cl::_sym_STARfeaturesSTAR->symbolValue().as<core::Cons_O>();
      cl::_sym_STARfeaturesSTAR->defparameter(core::Cons_O::create(mpi, features));
      core::_sym_STARmpi_rankSTAR->defparameter(core::make_fixnum(mpiRank));
      core::_sym_STARmpi_sizeSTAR->defparameter(core::make_fixnum(mpiSize));
    }
#endif
  
  }

    //
  // If --addresses was passed as a command line option - dump the addresses here
  //
  maybeHandleAddressesOption(core::global_options);
  
  if ( core::initializer_functions_are_waiting() ) {
    core::initializer_functions_invoke();
  }
  
#ifdef DEBUG_PROGRESS
  printf("%s:%d run\n", __FILE__, __LINE__ );
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
#ifdef USE_USER_SIGNAL
      gctools::wait_for_user_signal("Paused at startup");
#else
      printf("%s:%d PID = %d  Paused at startup - press enter to continue: \n", __FILE__, __LINE__, getpid() );
      fflush(stdout);
      getchar();
#endif
    }
  }
  // The system is fully up now
  _lisp->_Roots._TheSystemIsUp = true;
  core::Package_sp cluser = gc::As<core::Package_sp>(_lisp->findPackage("COMMON-LISP-USER"));
  cl::_sym_STARpackageSTAR->defparameter(cluser);

}

int run_clasp( gctools::ClaspInfo* claspInfo ) {
  int exitCode;
  if ( claspInfo->_loadSnapshotFile ||          // We want to load a snapshot
       (claspInfo->_start_of_snapshot != NULL)) // We found an embedded snapshot
  {
    _lisp->parseCommandLineArguments(*core::global_options);
    try {
      if (ext::_sym_STARsnapshot_save_load_startupSTAR->symbolValue().notnilp()) {
        core::T_sp fn = ext::_sym_STARsnapshot_save_load_startupSTAR->symbolValue();
        core::eval::funcall(fn);
      } else {
        core::write_bf_stream(fmt::sprintf("Clasp (copyright Christian E. Schafmeister 2014)\n"));
        core::write_bf_stream(fmt::sprintf("ext:*snapshot-save-load-startup* is nil so dropping into a simple repl\n"));
        core::write_bf_stream(fmt::sprintf("Low level repl\n"));
        _lisp->readEvalPrintInteractive();
        core::write_bf_stream(fmt::sprintf("\n"));
      }
    } catch (core::ExitProgramException &ee) {
      exitCode = ee.getExitResult();
    }
    snapshotSaveLoad::global_InSnapshotLoad = false;

#else
    printf("Core image loading is not supported unless precise GC is turned on\n");
#endif
  } else {
    try {
      exitCode = _lisp->run();
    } catch (core::SaveLispAndDie& ee) {
#ifdef USE_PRECISE_GC
      snapshotSaveLoad::snapshot_save(ee);
#endif
      exitCode = 0;
    }
  }
  return exitCode;
};

void shutdown_clasp()
{
  _lisp->_Roots._ClaspJIT = nil<core::T_O>();
  mp::ClaspThreads_exit(); // run pthreads_exit
  #ifdef USE_MPI
  if (!options._DisableMpi) {
    mpip::Mpi_O::Finalize();
  }
#endif

};

};
