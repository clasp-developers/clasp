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

#include <string>
#include <algorithm>
#include <cstdlib>
#include <cstdio>

#if defined( _TARGET_OS_LINUX ) || defined( _TARGET_OS_DARWIN )
#include <signal.h>
#include <sys/resource.h>
#include <libgen.h>
#include <execinfo.h>
#include <cxxabi.h>
#endif

#ifdef USE_MPI
#include <boost/mpi.hpp>
#endif

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
#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/clbind/clbindPackage.h>
#include <clasp/sockets/socketsPackage.h>
#include <clasp/serveEvent/serveEventPackage.h>
#include <clasp/asttooling/asttoolingPackage.h>
#include <clasp/core/pathname.h>
#ifdef USE_MPI
#include <clasp/mpip/mpiPackage.h>
#include <clasp/mpip/claspMpi.h>
#endif

// ---------------------------------------------------------------------------
// GLOBAL VARS
// ---------------------------------------------------------------------------

static const std::string CLASP_DEFAULT_PROGRAM_NAME( "clasp");
static const std::string CLASP_DEFAULT_EXE_NAME( CLASP_DEFAULT_PROGRAM_NAME );

static std::string g_exe_name;      // filename of the executable
static std::string g_program_name;  // logical / settable program name
static int         g_exit_code;     // exit code to be return at exit

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

// EXIT CODE HANDLING

void set_exit_code( int exit_code = EXIT_SUCCESS )
{
  g_exit_code = exit_code;
}

int exit_code( void )
{
  return g_exit_code;
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
  printf("%s:%d There was an unhandled exception - do something about it.\n", __FILE__, __LINE__ );
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
  core::LispHolder lispHolder(mpiEnabled, mpiRank, mpiSize);
  int exit_code = 0;
  
  printf("%s:%d Entered try block typeid(ExitProgramException) -> %lu\n", __FILE__, __LINE__, typeid(core::ExitProgramException).hash_code());
  printf("%s:%d   again try block typeid(ExitProgramException) -> %lu\n", __FILE__, __LINE__, typeid(core::ExitProgramException).hash_code());
  gctools::GcToolsExposer_O GcToolsPkg(_lisp);
  clbind::ClbindExposer_O ClbindPkg(_lisp);
  llvmo::LlvmoExposer_O llvmopkg(_lisp);
  sockets::SocketsExposer_O SocketsPkg(_lisp);
  serveEvent::ServeEventExposer_O ServeEventPkg(_lisp);
  asttooling::AsttoolingExposer_O AsttoolingPkg(_lisp);

  lispHolder.startup(argc, argv, program_name().c_str() ); // was "CANDO_APP"

  _lisp->installPackage(&GcToolsPkg);
  _lisp->installPackage(&ClbindPkg);
  _lisp->installPackage(&llvmopkg);
  _lisp->installPackage(&SocketsPkg);
  _lisp->installPackage(&ServeEventPkg);
  _lisp->installPackage(&AsttoolingPkg);

#ifdef USE_MPI
  mpip::MpiExposer TheMpiPkg(_lisp);
  _lisp->installPackage(&TheMpiPkg);
  if (mpiEnabled) {
    core::Symbol_sp mpi = _lisp->internKeyword("MPI-ENABLED");
    core::Cons_sp features = cl::_sym_STARfeaturesSTAR->symbolValue().as<core::Cons_O>();
    cl::_sym_STARfeaturesSTAR->defparameter(core::Cons_O::create(mpi, features));
  } else {
    SIMPLE_ERROR(BF("USE_MPI is true but mpiEnabled is false!!!!"));
  }
#endif

    // printf("%s:%d About to _lisp->run() - ExitProgram typeid %p;\n",
    // __FILE__, __LINE__, (void*)&typeid(core::ExitProgram) );
    // RUN THIS LISP IMPLEMENTATION
  exit_code = _lisp->run();
  if ( exit_code != 0 )
  {
    fprintf( stderr, "*** ERROR: %s is terminating with exit code %d !\n",
             program_name().c_str(), exit_code );
  }
  set_exit_code( exit_code );
  return exit_code;

} // STARTUP

// -------------------------------------------------------------------------
//     M A I N
// -------------------------------------------------------------------------


void* to_fixnum(int8_t v) {
    return reinterpret_cast<void*>(((Fixnum)v) << 2);
}



int main( int argc, char *argv[] )
{
#if 0
    int8_t array[1] = {-3};
    //    printf("sizeof(array[1]) -> %lu\n", sizeof(array));
    int8_t v = array[0];
    // printf("int8_t v -> %x\n", v);
    void* foo = to_fixnum(v);
    ///printf("int64_t v -> %p\n", foo);
#endif

  // ------------
  uintptr_clasp_t x = 0;
  uintptr_t y = 1;
  x = y;
  y = x;
#if 0
   // A few tests - delete them when not needed anymore
  test_va_list(NULL,6
               ,core::clasp_make_fixnum(0).raw_()
               ,core::clasp_make_fixnum(1).raw_()
               ,core::clasp_make_fixnum(2).raw_()
               ,core::clasp_make_fixnum(3).raw_()
               ,core::clasp_make_fixnum(4).raw_()
               ,core::clasp_make_fixnum(5).raw_());
#endif
  // Do not touch debug log until after MPI init

  bool mpiEnabled = false;
  int  mpiRank    = 0;
  int  mpiSize    = 1;

  // DO BASIC EXE SETUP

  set_exit_code(); // Set exit code to default value
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
  rlimit rl_new;

  rl.rlim_max = 16 * 1024 * 1024; // 16 MB
  rl.rlim_cur = 15 * 1024 * 1024; // 15 MB

  int rc = setrlimit( RLIMIT_STACK, &rl );
  if ( rc != 0 )
  {
    fprintf( stderr, "*** %s (%s:%d): WARNING: Could not set stack size as requested (error code %d - rlim_max = %lu) !\n",
             exe_name().c_str(), __FILE__, __LINE__, rc, (unsigned long)rl.rlim_max );
  }

  getrlimit( RLIMIT_STACK, &rl_new );
  if( rl.rlim_max != rl_new.rlim_max )
  {
    fprintf( stderr, "*** %s (%s:%d): WARNING: Could not set stack size as requested (error code %d - rlim_max = %lu) !\n",
             exe_name().c_str(), __FILE__, __LINE__, EXIT_FAILURE, (unsigned long)rl_new.rlim_max );
  }

  // - MPI ENABLEMENT

#ifdef USE_MPI
  try
  {
    mpip::Mpi_O::Init(argc, argv, mpiEnabled, mpiRank, mpiSize);
  }
  catch ( core::HardError &err )
  {
    fprintf( stderr, "**** %s (%s:%d): ERROR: Could not start MPI - ABORTING!\n",
             exe_name().c_str(), __FILE__, __LINE__ );
    abort();
  }
#endif

  // - THREAD SETUP - FOR CANDO -> TODO: Move this into CANDO extension
  // frgo, 2016-11-13
  //
  // int maxThreads = 1;
  // {
  //   maxThreads = core::cando_omp_get_num_threads();
  // }

  // SAY HELLO

  // fprintf( stderr, "%s (%s:%d) - started with stack size set to %llu bytes.\n", exe_name().c_str(), __FILE__, __LINE__, rl_new.rlim_max );

  fflush( stderr );

  // - COMMAND LINE OPTONS HANDLING

#if 0
  // Use this to check if smart_ptr<core::T_O> is being passed by value or reference
  gctools::smart_ptr<core::T_O> x((gctools::Tagged)0xDEADBEEF);
  foo(x);
#endif
  
  core::CommandLineOptions options(argc, argv);

  // CALL LISP STARTUP

  int exit_code = 0;
  try
  {
    exit_code = gctools::startupGarbageCollectorAndSystem( &startup, argc, argv, rl.rlim_max, mpiEnabled, mpiRank, mpiSize );
    set_exit_code( exit_code );
  }
  catch (const core::DynamicGo& go) {
    fprintf(stderr, "%s:%d  Uncaught DynamicGo\n", __FILE__, __LINE__ );
#if defined(DEBUG_FLOW_TRACKER)
    core::Cons_sp throwHandleCons((gc::Tagged)go.getHandle());
    if (!throwHandleCons.consp()) printf("%s:%d The throwHandleCons is not a CONS -> %p\n", __FILE__, __LINE__, (void*)throwHandleCons.raw_() );
    Fixnum throwFlowCounter = CONS_CAR(throwHandleCons).unsafe_fixnum();
    printf("%s:%d A GO has missed its target frame - the GO has the handle %p and is looking for FlowCounter %" PFixnum " and it reached main()\n", __FILE__, __LINE__, throwHandleCons.raw_(), throwFlowCounter );
    flow_tracker_last_throw_backtrace_dump();
#endif
    abort();
  }
  catch (const core::ReturnFrom& rf) {
    fprintf(stderr, "%s:%d  Uncaught ReturnFrom\n", __FILE__, __LINE__ );
#if defined(DEBUG_FLOW_TRACKER)
    core::Cons_sp throwHandleCons((gc::Tagged)rf.getHandle());
    if (!throwHandleCons.consp()) printf("%s:%d The throwHandleCons is not a CONS -> %p\n", __FILE__, __LINE__, (void*)throwHandleCons.raw_() );
    Fixnum throwFlowCounter = CONS_CAR(throwHandleCons).unsafe_fixnum();
    printf("%s:%d A RETURN-FROM has missed its target frame - the RETURN-FROM has the handle %p and is looking for FlowCounter %" PFixnum " and it reached main()\n", __FILE__, __LINE__, throwHandleCons.raw_(), throwFlowCounter );
    flow_tracker_last_throw_backtrace_dump();
#endif
    abort();
  }

  catch ( ... )
  {
    // As we don't know what went wrong we just exit with a generic error code
    fprintf( stderr, "**** %s (%s:%d): FATAL ERROR - ",
             exe_name().c_str(), __FILE__, __LINE__ );

    handle_unhandled_exception();

    fprintf( stderr, "**** %s (%s:%d): ABORTING!",
             exe_name().c_str(), __FILE__, __LINE__ );
    fflush( stderr );

    set_abort_flag( true );
  }

#ifdef USE_MPI
  mpip::Mpi_O::Finalize();
#endif

//  std::terminate(); // This calls the terminate handler and then exits or aborts!
}
