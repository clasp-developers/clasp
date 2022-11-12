/*
    File: main.cc
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
//#define DEBUG_LEVEL_FULL

// ---------------------------------------------------------------------------
//  CLASP FOUNDATION INCLUDE - HAS TO BE FIRST INCLUDE !!!
// ---------------------------------------------------------------------------

#include <clasp/core/foundation.h>

// ---------------------------------------------------------------------------
//  SYSTEM INCLUDES
// ---------------------------------------------------------------------------
#include <dlfcn.h>
#include <string>
#include <algorithm>
#include <cstdlib>
#include <cstdio>

#if defined( _TARGET_OS_LINUX ) || defined( _TARGET_OS_DARWIN ) || defined( _TARGET_OS_FREEBSD)
#include <signal.h>
#include <sys/resource.h>
#include <libgen.h>
#include <execinfo.h>
#include <cxxabi.h>
#endif

#ifdef USE_MPI
#include <boost/mpi.hpp>
#endif


#include "llvm/Support/CommandLine.h"

// ---------------------------------------------------------------------------
//  CLASP INCLUDES
// ---------------------------------------------------------------------------

#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/bundle.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/array.h>
#include <clasp/core/function.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/candoOpenMp.h>
#include <clasp/core/cons.h>
#include <clasp/core/commandLineOptions.h>
#include <clasp/core/instance.h>
#ifdef DEBUG_DYN_ENV_STACK
#include <clasp/core/unwind.h> // global_debug_dyn_env_stack
#endif
#include <clasp/llvmo/llvmoPackage.h>
#include <clasp/core/debugger.h>
#include <clasp/core/posixTime.h>
#include <clasp/core/primitives.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/clbind/clbindPackage.h>
#include <clasp/sockets/socketsPackage.h>
#include <clasp/serveEvent/serveEventPackage.h>
#include <clasp/asttooling/asttoolingPackage.h>
#include <clasp/gctools/snapshotSaveLoad.h>
#include <clasp/gctools/interrupt.h>
#include <clasp/core/pathname.h>
#include <clasp/clbind/open.h>
#include <clasp/core/compiler.h>
#include <clasp/gctools/gc_interface.fwd.h>
#ifdef USE_MPI
#include <clasp/mpip/mpiPackage.h>
#include <clasp/mpip/claspMpi.h>
#endif

#ifdef _TARGET_OS_DARWIN
#include <mach-o/getsect.h>
#endif


#ifndef SCRAPING
#define ALL_EXPOSES_EXTERN
#include EXPOSE_INC_H
#undef ALL_EXPOSES_EXTERN
#endif


namespace llvmo {
void initialize_llvm(int argc, char **argv);

};

// ---------------------------------------------------------------------------
// IMPLEMENTATION
// ---------------------------------------------------------------------------

// PRINT STACKTRACE PROGRAMMICALLY


static inline void print_stacktrace(FILE *out = stderr, unsigned int max_frames = 63)
{
    fprintf(out, "stack trace:\n");

    // storage array for stack trace address data
    void* addrlist[max_frames+1];

    // retrieve current stack addresses
    int addrlen = backtrace(addrlist, sizeof(addrlist) / sizeof(void*));

    if (addrlen == 0) {
	fprintf(out, "  <empty, possibly corrupt>\n");
	return;
    }

    // resolve addresses into strings containing "filename(function+address)",
    // this array must be free()-ed
    char** symbollist = backtrace_symbols(addrlist, addrlen);

    // allocate string which will be filled with the demangled function name
    size_t funcnamesize = 256;
    char* funcname = (char*)malloc(funcnamesize);

    // iterate over the returned symbol lines. skip the first, it is the
    // address of this function.
    for (int i = 1; i < addrlen; i++)
    {
	char *begin_name = 0, *begin_offset = 0, *end_offset = 0;

	// find parentheses and +address offset surrounding the mangled name:
	// ./module(function+0x15c) [0x8048a6d]
	for (char *p = symbollist[i]; *p; ++p)
	{
	    if (*p == '(')
		begin_name = p;
	    else if (*p == '+')
		begin_offset = p;
	    else if (*p == ')' && begin_offset) {
		end_offset = p;
		break;
	    }
	}

	if (begin_name && begin_offset && end_offset
	    && begin_name < begin_offset)
	{
	    *begin_name++ = '\0';
	    *begin_offset++ = '\0';
	    *end_offset = '\0';

	    // mangled name is now in [begin_name, begin_offset) and caller
	    // offset in [begin_offset, end_offset). now apply
	    // __cxa_demangle():

	    int status;
	    char* ret = abi::__cxa_demangle(begin_name,
					    funcname, &funcnamesize, &status);
	    if (status == 0) {
		funcname = ret; // use possibly realloc()-ed string
		fprintf(out, "  %s : %s+%s\n",
			symbollist[i], funcname, begin_offset);
	    }
	    else {
		// demangling failed. Output function name as a C function with
		// no arguments.
		fprintf(out, "  %s : %s()+%s\n",
			symbollist[i], begin_name, begin_offset);
	    }
	}
	else
	{
	    // couldn't parse the line? print the whole line.
	    fprintf(out, "  %s\n", symbollist[i]);
	}
    }

    free(funcname);
    free(symbollist);
}

// ABORT FLAG HANDLING

// EXCEPTION HANDLING

void handle_unhandled_exception( void )
{
  // This is a trick to get info about the last thrown exception.
  // This was advertized on Stackoverflow.
  
  try
  {
    throw;
  }
  catch ( const std::exception &e )
  {
    fprintf( stderr, "Unhandled ecception: %s", e.what() );
  }
  catch ( ... )
  {
    fprintf( stderr, "Unknown unhandled ecception" );
  }
}

// STARTUP HANDLING

static int startup( gctools::ClaspInfo* claspInfo )
{
  
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
  void* start_of_snapshot = getsectiondata(exec_header,
                                           SNAPSHOT_SEGMENT,
                                           SNAPSHOT_SECTION,
                                           &size);
  void* end_of_snapshot = NULL;
  if (start_of_snapshot) {
    end_of_snapshot = (void*)((char*)start_of_snapshot + size);
  }
#  endif
#  ifdef _TARGET_OS_LINUX
  void* start_of_snapshot = NULL;
  void* end_of_snapshot = NULL;
  extern const char __attribute__((weak)) SNAPSHOT_START;
  extern const char __attribute__((weak)) SNAPSHOT_END;
  start_of_snapshot = (void*)&SNAPSHOT_START;
  end_of_snapshot = (void*)&SNAPSHOT_END;
#  endif
#else
  void* start_of_snapshot = NULL;
  void* end_of_snapshot = NULL;
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
  bool loadSnapshotFile = false;
#ifdef USE_PRECISE_GC  
  std::string snapshotFileName = "";
  if (!start_of_snapshot) {
    if (core::global_options->_StartupFileP &&
        core::global_options->_StartupFileType == core::cloSnapshot) {
      snapshotFileName = core::global_options->_StartupFile;
      loadSnapshotFile = true;
    } else if (core::global_options->_DefaultStartupType == core::cloDefault) {
      snapshotFileName = core::startup_snapshot_name(*bundle);
      if (std::filesystem::exists(std::filesystem::path(snapshotFileName))) {
        loadSnapshotFile = true;
      }
    } else if (core::global_options->_DefaultStartupType == core::cloSnapshot) {
      snapshotFileName = core::startup_snapshot_name(*bundle);
      if (std::filesystem::exists(std::filesystem::path(snapshotFileName))) {
        loadSnapshotFile = true;
      } else {
        printf("Could not find snapshot file %s - exiting.\n", snapshotFileName.c_str());
        exit(1);
      }
    }
  }
#endif
  if ( loadSnapshotFile ||          // We want to load a snapshot
       (start_of_snapshot != NULL) // We found an embedded snapshot
       ) {
    //
    // Load a snapshot from a file (loadSnapshotFile=true) or from memory (start_of_snapshot!=NULL)
    //
#ifdef USE_PRECISE_GC
    if (loadSnapshotFile && core::startup_snapshot_is_stale(snapshotFileName)) {
      printf("The startup snapshot file \"%s\" is stale - remove it or create a new one\n", snapshotFileName.c_str() );
      std::exit(1);
    }
    llvmo::initialize_llvm();

    clbind::initializeCastGraph();
    if (start_of_snapshot) {
      core::global_startupSourceName = "memory";
      core::global_startupEnum = core::snapshotMemory;
    } else {
      core::global_startupSourceName = snapshotFileName;
      core::global_startupEnum = core::snapshotFile;
    }
    exit_code = snapshotSaveLoad::snapshot_load( (void*)start_of_snapshot, (void*)end_of_snapshot, snapshotFileName );
#else
    printf("Core image loading is not supported unless precise GC is turned on\n");
#endif
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

    //
    // Run lisp
    //
    exit_code = _lisp->run();

    //
    // Shutdown lisp
    //
    _lisp->uninstallPackage(&AsttoolingPkg);
    _lisp->uninstallPackage(&ServeEventPkg);
    _lisp->uninstallPackage(&SocketsPkg);
    _lisp->uninstallPackage(&llvmopkg);
    _lisp->uninstallPackage(&ClbindPkg);
    _lisp->uninstallPackage(&GcToolsPkg);

  }
  return exit_code;
}

// -------------------------------------------------------------------------
//     M A I N
// -------------------------------------------------------------------------



int main( int argc, const char *argv[] )
{
  // - SET STACK SIZE
  rlimit rl;
  getrlimit(RLIMIT_STACK,&rl);
  //printf("%s:%d cur: %lu max %lu\n", __FILE__, __LINE__ , (unsigned long) rl.rlim_cur, (unsigned long) rl.rlim_max);
  // Only set the limits if current values are lower
  if (rl.rlim_cur < CLASP_DESIRED_STACK_CUR) {
    rl.rlim_cur = CLASP_DESIRED_STACK_CUR;      
    int rc = setrlimit(RLIMIT_STACK, &rl);
    if (rc != 0) {
      fprintf(stderr, "*** %s (%s:%d): WARNING: Could not set stack size as requested (error code %d errno %d - rlim_cur %lu- rlim_max= %lu) !\n",
              gctools::exe_name().c_str(), __FILE__, __LINE__, rc, errno, (unsigned long) rl.rlim_cur, (unsigned long) rl.rlim_max);
    }
  }
  getrlimit(RLIMIT_STACK, &rl);

  gctools::ClaspInfo claspInfo( argc, argv, rl.rlim_cur );
  void* stackMarker = &stackMarker;
  startup_clasp( &stackMarker, &claspInfo );

  int exit_code = gctools::run_clasp( &startup, &claspInfo );

  gctools::shutdown_clasp();

  return exit_code;
}
