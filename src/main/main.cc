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

#if defined( _TARGET_OS_LINUX ) || defined( _TARGET_OS_DARWIN )
#include <signal.h>
#include <sys/resource.h>
#include <libgen.h>
#endif

#ifdef USE_MPI
#include <boost/mpi.hpp>
#endif

// ---------------------------------------------------------------------------
//  CLASP INCLUDES
// ---------------------------------------------------------------------------

#include <clasp/core/bundle.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/array.h>
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
  std::transform( g_program_name.begin(), g_program_name.end(), g_program_name.begin(),
                  [](unsigned char c) { return std::toupper(c); } );
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

  exit( exit_code() );
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

  try
  {
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

    _lisp->run();
  }
  catch (core::DynamicGo &failedGo)
  {
    fprintf( stderr,
             "%s (%s:%d): A DynamicGo was thrown but not caught - frame[%lu] tag[%lu]\n",
             exe_name().c_str(), __FILE__, __LINE__, failedGo.getFrame(), failedGo.index());
    exit_code = EXIT_FAILURE;
  }
  catch (core::Unwind &failedUnwind)
  {
    ASSERT(gctools::tagged_fixnump(failedUnwind.getFrame()));
    fprintf( stderr,
             "%s (%s:%d): An unwind was thrown but not caught - frame[%ld] tag[%lu]\n",
             exe_name().c_str(), __FILE__, __LINE__, gctools::untag_fixnum( failedUnwind.getFrame() ), failedUnwind.index() );
    exit_code = EXIT_FAILURE;
  }
  catch (core::ExitProgram &ee)
  {
    exit_code = ee.getExitResult();
  }
  catch (...)
  {
    fprintf( stderr,
             "%s (%s:%d): The generic catch(...) caught an unhandled exception of type %p - this is a bug. Please report this to %s support !\n",
             exe_name().c_str(), __FILE__, __LINE__, (void*)__cxxabiv1::__cxa_current_exception_type(),
             program_name().c_str() );
  }

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

int main( int argc, char *argv[] )
{
  // Do not touch debug log until after MPI init

  int rc = 0;
  int exit_code = EXIT_SUCCESS;

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

  rc = setrlimit( RLIMIT_STACK, &rl );
  if ( rc != 0 )
  {
    fprintf( stderr, "*** %s (%s:%d): WARNING: Could not set stack size as requested (error code %d - rlim_max = %llu) !\n",
             exe_name().c_str(), __FILE__, __LINE__, rc, rl.rlim_max );
  }

  getrlimit( RLIMIT_STACK, &rl_new );
  if( rl.rlim_max != rl_new.rlim_max )
  {
    fprintf( stderr, "*** %s (%s:%d): WARNING: Could not set stack size as requested (error code %d - rlim_max = %llu) !\n",
             exe_name().c_str(), __FILE__, __LINE__, EXIT_FAILURE, rl_new.rlim_max );
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

  core::CommandLineOptions options(argc, argv);

  // CALL LISP STARTUP

  try
  {
    exit_code = gctools::startupGarbageCollectorAndSystem( &startup, argc, argv, rl.rlim_max, mpiEnabled, mpiRank, mpiSize );
    set_exit_code( exit_code );
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

  std::terminate(); // This calls the terminate handler and then exits or aborts!
}
