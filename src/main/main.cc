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
#include <clasp/core/functor.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/candoOpenMp.h>
#include <clasp/core/cons.h>
#include <clasp/core/commandLineOptions.h>
#include <clasp/core/instance.h>
#include <clasp/llvmo/llvmoPackage.h>
#include <clasp/core/debugger.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/clbind/clbindPackage.h>
#include <clasp/sockets/socketsPackage.h>
#include <clasp/serveEvent/serveEventPackage.h>
#include <clasp/asttooling/asttoolingPackage.h>
#include <clasp/gctools/imageSaveLoad.h>
#include <clasp/gctools/interrupt.h>
#include <clasp/core/pathname.h>
#include <clasp/clbind/open.h>
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
// GLOBAL VARS
// ---------------------------------------------------------------------------

#define CLASP_DEFAULT_PROGRAM_NAME "clasp"
#define CLASP_DEFAULT_EXE_NAME CLASP_DEFAULT_PROGRAM_NAME

static std::string g_exe_name;      // filename of the executable
static std::string g_program_name;  // logical / settable program name

static bool        g_abort_flag;

static std::terminate_handler g_prev_terminate_handler;

// ---------------------------------------------------------------------------
// IMPLEMENTATION
// ---------------------------------------------------------------------------

// PRINT STACKTRACE PROGRAMMICALLY

void test_va_list( LCC_ARGS_ELLIPSIS ) {
  INITIALIZE_VA_LIST(); // now lcc_vargs is a VaList_sp over the arguments
  printf("%s:%d    relative arg#%d: %p  \n", __FILE__, __LINE__, 0, lcc_vargs->relative_indexed_arg(0));
  printf("%s:%d    relative arg#%d: %p  \n", __FILE__, __LINE__, 1, lcc_vargs->relative_indexed_arg(1));
  printf("%s:%d    relative arg#%d: %p  \n", __FILE__, __LINE__, 2, lcc_vargs->relative_indexed_arg(2));
  printf("%s:%d    relative arg#%d: %p  \n", __FILE__, __LINE__, 3, lcc_vargs->relative_indexed_arg(3));
  printf("%s:%d    relative arg#%d: %p  \n", __FILE__, __LINE__, 4, lcc_vargs->relative_indexed_arg(4));
  printf("%s:%d    relative arg#%d: %p  \n", __FILE__, __LINE__, 5, lcc_vargs->relative_indexed_arg(5));
  printf("%s:%d    Advanced args by one\n", __FILE__, __LINE__ );
  lcc_vargs->next_arg_raw();
  printf("%s:%d    relative arg#%d: %p   va_arg: %p\n", __FILE__, __LINE__, 0, lcc_vargs->relative_indexed_arg(0), lcc_vargs->next_arg_raw());
  printf("%s:%d    relative arg#%d: %p   va_arg: %p\n", __FILE__, __LINE__, 1, lcc_vargs->relative_indexed_arg(0), lcc_vargs->next_arg_raw());
  printf("%s:%d    relative arg#%d: %p   va_arg: %p\n", __FILE__, __LINE__, 2, lcc_vargs->relative_indexed_arg(0), lcc_vargs->next_arg_raw());
  printf("%s:%d    relative arg#%d: %p   va_arg: %p\n", __FILE__, __LINE__, 3, lcc_vargs->relative_indexed_arg(0), lcc_vargs->next_arg_raw());
  printf("%s:%d    relative arg#%d: %p   va_arg: %p\n", __FILE__, __LINE__, 4, lcc_vargs->relative_indexed_arg(0), lcc_vargs->next_arg_raw());
}

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
//  printf("%s:%d:%s program_name = %s\n", __FILE__, __LINE__, __FUNCTION__, program_name.c_str());
  g_program_name = program_name;
#if 0
  std::transform( g_program_name.begin(), g_program_name.end(), g_program_name.begin(),
                  [](unsigned char c) { return std::toupper(c); } );
#endif
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

  if( abort_flag() )
    abort();
#if DEBUG_FLOW_TRACKER
  flow_tracker_last_throw_backtrace_dump();
#endif
  try { throw; }
  catch (const std::exception& e) {
      fprintf(stderr, "%s:%d There was an unhandled std::exception in process [pid: %d] e.what()=[%s] - do something about it.\n", __FILE__, __LINE__, getpid(), e.what()  );
  } catch (...) {
      fprintf(stderr, "%s:%d There was an unhandled unknown exception in process [pid: %d] - do something about it.\n", __FILE__, __LINE__, getpid() );
  };
  abort();
}

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

static int startup(int argc, char *argv[], bool &mpiEnabled, int &mpiRank, int &mpiSize)
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


  if (getenv("CLASP_DEBUGGER_SUPPORT")) {
    printf("%s:%d:%s  Generating clasp object layouts\n", __FILE__, __LINE__, __FUNCTION__ );
    stringstream ss;
    char* username = getenv("USER");
    if (!username) {
      printf("Could not get USER environment variable\n");
      exit(1);
    }
    ss << "/tmp/clasp_layouts_" << getenv("USER");
    core::dumpDebuggingLayouts(ss.str());
  }

    // Create the one global CommandLineOptions object and do some minimal argument processing
  core::global_options = new core::CommandLineOptions(argc, argv);
  (core::global_options->_ProcessArguments)(core::global_options);
  ::globals_ = new core::globals_t();
  globals_->_ExportedSymbolsAccumulate = core::global_options->_ExportedSymbolsAccumulate;
  globals_->_ExportedSymbolsFilename = core::global_options->_ExportedSymbolsFilename;
  globals_->_DebugStream = new core::DebugStream(mpiRank);
#ifdef USE_PRECISE_GC
#  ifdef _TARGET_OS_DARWIN
    const struct mach_header_64 * exec_header = (const struct mach_header_64 *)dlsym(RTLD_DEFAULT,"_mh_execute_header");
    size_t size;
    void* start_of_snapshot = getsectiondata(exec_header,
                                             "__CLASP",
                                             "__clasp",
                                             &size);
    void* end_of_snapshot = NULL;
    if (start_of_snapshot) {
      end_of_snapshot = (void*)((char*)start_of_snapshot + size);
    }
#  endif
#  ifdef _TARGET_OS_LINUX
    void* start_of_snapshot = NULL;
    void* end_of_snapshot = NULL;
    extern const char __attribute__((weak)) _binary_extensions_cando_generated_cando_snapshot_start;
    extern const char __attribute__((weak)) _binary_extensions_cando_generated_cando_snapshot_end;
    start_of_snapshot = (void*)&_binary_extensions_cando_generated_cando_snapshot_start;
    end_of_snapshot = (void*)&_binary_extensions_cando_generated_cando_snapshot_end;
    if (start_of_snapshot) {
      //printf("%s:%d:%s embedded snapshot %p *snapshot -> %p\n", __FILE__, __LINE__, __FUNCTION__, start_of_snapshot, *(void**)start_of_snapshot );
    } else {
//      printf("%s:%d:%s embedded snapshot %p \n", __FILE__, __LINE__, __FUNCTION__, start_of_snapshot );
    }
#  endif
#else
    void* start_of_snapshot = NULL;
    void* end_of_snapshot = NULL;
#endif
  if (!core::global_options->_DontLoadImage && // YES load the image
      ( core::global_options->_ImageType == core::cloSnapshot // YES its a snapshot
        || start_of_snapshot != NULL) // We found an embedded snapshot
        ) {
#ifdef USE_PRECISE_GC
    llvmo::initialize_llvm();

    //
    // Set up the arguments
    //
    const char *argv0 = "./";
    if (argc > 0) argv0 = argv[0];
    globals_->_Argc = argc;
    for (int i = 0; i < argc; ++i) {
      globals_->_Argv.push_back(string(argv[i]));
    }
    //
    // Look around the local directories for source and fasl files.
    //
    core::Bundle *bundle = new core::Bundle(argv0,core::global_options->_ResourceDir);
    globals_->_Bundle = bundle;
    clbind::initializeCastGraph();
    exit_code = imageSaveLoad::image_load( (void*)start_of_snapshot, (void*)end_of_snapshot, core::global_options->_ImageFile );
#else
    printf("Core image loading is not supported unless precise GC is turned on\n");
#endif
  } else {
    core::LispHolder lispHolder(mpiEnabled, mpiRank, mpiSize);

    gctools::GcToolsExposer_O GcToolsPkg(_lisp);
    clbind::ClbindExposer_O ClbindPkg(_lisp);
    llvmo::LlvmoExposer_O llvmopkg(_lisp);
    sockets::SocketsExposer_O SocketsPkg(_lisp);
    serveEvent::ServeEventExposer_O ServeEventPkg(_lisp);
    asttooling::AsttoolingExposer_O AsttoolingPkg(_lisp);

    std::string progname = program_name();
    lispHolder.startup(core::global_options, argc, argv, progname.c_str()); // was "CANDO_APP"

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
  
    // printf("%s:%d About to _lisp->run() - ExitProgram typeid %p;\n",
    // __FILE__, __LINE__, (void*)&typeid(core::ExitProgram) );
    // RUN THIS LISP IMPLEMENTATION
    exit_code = _lisp->run();
  }
  return exit_code;

} // STARTUP

// -------------------------------------------------------------------------
//     M A I N
// -------------------------------------------------------------------------


void* to_fixnum(int8_t v) {
    return reinterpret_cast<void*>(((Fixnum)v) << 2);
}

#define clasp_desired_stack_cur 16 * 1024 * 1024


int main( int argc, char *argv[] )
{
#if 0
  const char* bogus_args[3] = {"clasp","--debug-only", "jitlink"};
  llvm::cl::ParseCommandLineOptions(3,bogus_args,"clasp");
#endif

  const char* dof = getenv("CLASP_DEBUG_OBJECT_FILES");
  if (dof) {
    if (strcmp(dof,"save")==0) {
      llvmo::globalDebugObjectFiles = llvmo::DebugObjectFilesPrintSave;
    } else {
      llvmo::globalDebugObjectFiles = llvmo::DebugObjectFilesPrint;
    }
  }
  
  // Do not touch debug log until after MPI init

  bool mpiEnabled = false;
  int  mpiRank    = 0;
  int  mpiSize    = 1;


  // DO BASIC EXE SETUP

  set_abort_flag(); // Set abort flag to default value
  g_prev_terminate_handler = std::set_terminate( [](){ clasp_terminate_handler(); } );

  // - STORE NAME OF EXECUTABLE

  {
    std::string exename( argv[ 0 ] );
    set_exe_name( basename( (char *) exename.c_str() ) );
  }

  // - SET THE APPLICATION NAME

  set_program_name();

  // - SET STACK SIZE
  rlimit rl;
  getrlimit(RLIMIT_STACK,&rl);
  //printf("%s:%d cur: %lu max %lu\n", __FILE__, __LINE__ , (unsigned long) rl.rlim_cur, (unsigned long) rl.rlim_max);
  // Only set the limits if current values are lower
  if (rl.rlim_cur < clasp_desired_stack_cur) {
    rl.rlim_cur = clasp_desired_stack_cur;      
    int rc = setrlimit(RLIMIT_STACK, &rl);
    if (rc != 0)
    {
      fprintf(stderr, "*** %s (%s:%d): WARNING: Could not set stack size as requested (error code %d errno %d - rlim_cur %lu- rlim_max= %lu) !\n",
              exe_name().c_str(), __FILE__, __LINE__, rc, errno, (unsigned long) rl.rlim_cur, (unsigned long) rl.rlim_max);
    }
  }
  getrlimit(RLIMIT_STACK, &rl);
  //printf("%s:%d cur: %lu max %lu\n", __FILE__, __LINE__ , (unsigned long) rl.rlim_cur, (unsigned long) rl.rlim_max);
  
  // - COMMAND LINE OPTONS HANDLING

  core::CommandLineOptions options(argc, argv);

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

  // Pause before any allocations take place
  {

    if (getenv("CLASP_DEBUGGER_SUPPORT")) {
      printf("%s:%d:%s  Setting up clasp for debugging - writing PID to /tmp/clasp_pid\n", __FILE__, __LINE__, __FUNCTION__);
      stringstream ss;
      char* username = getenv("USER");
      if (!username) {
        printf("%s:%d:%s Could not get USER environment variable\n", __FILE__, __LINE__, __FUNCTION__ );
        exit(1);
      }
      ss << "/tmp/clasp_pid_" << getenv("USER");
      FILE* fout = fopen(ss.str().c_str(),"w");
      if (!fout) {
        printf("%s:%d:%s Could not open %s\n", __FILE__, __LINE__, __FUNCTION__, ss.str().c_str() );
        exit(1);
      }
      fprintf(fout,"%d",getpid());
      fclose(fout);
    }
    char* pause_startup = getenv("CLASP_PAUSE_STARTUP");
    if (pause_startup) {
#ifdef USE_USER_SIGNAL
      gctools::setup_user_signal();
      gctools::wait_for_user_signal("Paused at startup before all initialization");
#else
      printf("%s:%d PID = %d  Paused at startup before all initialization - press enter to continue: \n", __FILE__, __LINE__, getpid() );
      fflush(stdout);
      getchar();
#endif
    }
  }

  // Startup the garbage collector and the lisp system
  int exit_code;
  {
    exit_code = gctools::startupGarbageCollectorAndSystem( &startup, argc, argv, rl.rlim_cur, mpiEnabled, mpiRank, mpiSize );
  }
  
#ifdef USE_MPI
  if (!options._DisableMpi) {
    mpip::Mpi_O::Finalize();
  }
#endif

  return exit_code;
}
