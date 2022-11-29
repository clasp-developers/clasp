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

#if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN) || defined(_TARGET_OS_FREEBSD)
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

// ABORT FLAG HANDLING

// EXCEPTION HANDLING

void handle_unhandled_exception(void) {
  // This is a trick to get info about the last thrown exception.
  // This was advertized on Stackoverflow.

  try {
    throw;
  } catch (const std::exception &e) {
    fprintf(stderr, "Unhandled ecception: %s", e.what());
  } catch (...) {
    fprintf(stderr, "Unknown unhandled ecception");
  }
}

// -------------------------------------------------------------------------
//     M A I N
// -------------------------------------------------------------------------

int main(int argc, const char *argv[]) {
  // - SET STACK SIZE
  rlimit rl;
  getrlimit(RLIMIT_STACK, &rl);
  // printf("%s:%d cur: %lu max %lu\n", __FILE__, __LINE__ , (unsigned long) rl.rlim_cur, (unsigned long) rl.rlim_max);
  //  Only set the limits if current values are lower
  if (rl.rlim_cur < CLASP_DESIRED_STACK_CUR) {
    rl.rlim_cur = CLASP_DESIRED_STACK_CUR;
    int rc = setrlimit(RLIMIT_STACK, &rl);
    if (rc != 0) {
      fprintf(stderr,
              "*** %s (%s:%d): WARNING: Could not set stack size as requested (error code %d errno %d - rlim_cur %lu- rlim_max= "
              "%lu) !\n",
              gctools::exe_name().c_str(), __FILE__, __LINE__, rc, errno, (unsigned long)rl.rlim_cur, (unsigned long)rl.rlim_max);
    }
  }
  getrlimit(RLIMIT_STACK, &rl);

  gctools::ClaspInfo claspInfo(argc, argv, rl.rlim_cur);
  void *stackMarker = &stackMarker;
  int exit_code = 0;
  if (startup_clasp(&stackMarker, &claspInfo, &exit_code)) {
   exit_code = run_clasp(&claspInfo);
  }

  shutdown_clasp(&claspInfo);

  return exit_code;
}
