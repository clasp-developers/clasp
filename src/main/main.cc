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
#define DEBUG_LEVEL_FULL

#ifdef _TARGET_OS_LINUX
#include <signal.h>
#include <sys/resource.h>
#endif

#ifdef USE_MPI
#include <boost/mpi.hpp>
#endif
#include <string>
#include <clasp/core/foundation.h>
#include <clasp/core/bundle.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/str.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/candoOpenMp.h>
#include <clasp/core/cons.h>
#include <clasp/core/commandLineOptions.h>
#include <clasp/cffi/cffiPackage.h>
#include <clasp/core/instance.h>
#include <clasp/llvmo/llvmoPackage.h>
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

int startup(int argc, char *argv[], bool &mpiEnabled, int &mpiRank, int &mpiSize) {
  core::LispHolder lispHolder(mpiEnabled, mpiRank, mpiSize);
  int exitCode = 0;
  try {
#if 0
    // Set the ThreadInfo for the current master thread
    //
    core::ThreadInfo mainThreadInfo;
    core::lisp_setThreadLocalInfoPtr(&mainThreadInfo);
#endif
    
#if 1

    gctools::GcToolsExposer_O GcToolsPkg(_lisp);
    clbind::ClbindExposer_O ClbindPkg(_lisp);
    llvmo::LlvmoExposer_O llvmopkg(_lisp);
    cffi::CffiExposer_O cffipkg(_lisp);
    sockets::SocketsExposer_O SocketsPkg(_lisp);
    serveEvent::ServeEventExposer_O ServeEventPkg(_lisp);
    asttooling::AsttoolingExposer_O AsttoolingPkg(_lisp);
    lispHolder.startup(argc, argv, "CLASP"); // was "CANDO_APP"
    _lisp->installPackage(&GcToolsPkg);
    _lisp->installPackage(&ClbindPkg);
    _lisp->installPackage(&llvmopkg);
    _lisp->installPackage(&cffipkg);
    _lisp->installPackage(&SocketsPkg);
    _lisp->installPackage(&ServeEventPkg);
    _lisp->installPackage(&AsttoolingPkg);

#else
    lispHolder.startup(argc, argv, "CLASP"); // was "CANDO_APP"
    gctools::GcToolsExposer GcToolsPkg(_lisp);
    _lisp->installPackage(&GcToolsPkg);

    clbind::ClbindExposer ClbindPkg(_lisp);
    _lisp->installPackage(&ClbindPkg);

    llvmo::LlvmoExposer llvmopkg(_lisp);
    _lisp->installPackage(&llvmopkg);

    cffi::CffiExposer cffipkg(_lisp);
    _lisp->installPackage(&cffipkg);

    sockets::SocketsExposer SocketsPkg(_lisp);
    _lisp->installPackage(&SocketsPkg);

    serveEvent::ServeEventExposer ServeEventPkg(_lisp);
    _lisp->installPackage(&ServeEventPkg);

    asttooling::AsttoolingExposer AsttoolingPkg(_lisp);
    _lisp->installPackage(&AsttoolingPkg);
#endif



    
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
    _lisp->run();
  } catch (core::DynamicGo &failedGo) {
    printf("%s:%d A DynamicGo was thrown but not caught frame[%lu] tag[%lu]\n", __FILE__, __LINE__, failedGo.getFrame(), failedGo.index());
  } catch (core::Unwind &failedUnwind) {
    ASSERT(gctools::tagged_fixnump(failedUnwind.getFrame()));
    printf("%s:%d An unwind was thrown but not caught frame[%ld] tag[%lu]\n", __FILE__, __LINE__, gctools::untag_fixnum(failedUnwind.getFrame()), failedUnwind.index());
  } catch (core::ExitProgram &ee) {
    printf("\n");
    //            printf("Caught ExitProgram in %s:%d\n", __FILE__, __LINE__);
    exitCode = ee.getExitResult();
  }; // catch (...) { exitCode = gctools::handleFatalCondition(); }
  return exitCode;
}


void create_source_main_host()
{
  core::Cons_sp pts =
    core::Cons_O::createList(core::Cons_O::createList(core::Str_O::create("source-main:**;*.*"),
                                                      cl__pathname(core::Str_O::create("app-resources:clasp;src;main;**;*.*")))
        /* ,  more here */
                       );
core__pathname_translations(core::Str_O::create("source-main"), _lisp->_true(), pts);
}

class DummyClass { };

class EmptyClass : public DummyClass { // : public gctools::GCObject {
private:
public:
  static core::Symbol_sp static_classSymbol() { return UNDEFINED_SYMBOL; };
  static void set_static_creator(gc::smart_ptr<core::Creator_O> cb){};

public:
  explicit EmptyClass();
};



int main(int argc, char *argv[]) { // Do not touch debug log until after MPI init
  // debugging stuff - delete if not needed
  if (std::getenv("NOASLR")!=NULL) {
    srand(1);
    printf("%s:%d setting srand(1)\n", __FILE__, __LINE__ );
    printf("%s:%d sizeof(EmptyClass) = %zu\n", __FILE__, __LINE__, sizeof(EmptyClass)); 
    printf("%s:%d sizeof(_RootDummyClass) = %zu\n", __FILE__, __LINE__, sizeof(_RootDummyClass));
    printf("%s:%d sizeof(T_O) = %lu\n", __FILE__, __LINE__, sizeof(core::T_O));
    printf("%s:%d sizeof(Cons_O) = %lu\n", __FILE__, __LINE__, sizeof(core::Cons_O));
  }
  rlimit rl;
  rl.rlim_max = 16 * 1024 * 1024;
  rl.rlim_cur = 15 * 1024 * 1024;
  setrlimit(RLIMIT_STACK, &rl);
  getrlimit(RLIMIT_STACK, &rl);

  bool mpiEnabled = false;
  int mpiRank = 0;
  int mpiSize = 1;
#ifdef USE_MPI
  try {
    mpip::Mpi_O::Init(argc, argv, mpiEnabled, mpiRank, mpiSize);
  } catch (core::HardError &err) {
    printf("Could not start MPI\n");
    exit(1);
  }
#endif
  int maxThreads = 1;
  {
    {
      maxThreads = core::cando_omp_get_num_threads();
    }
  }

  core::CommandLineOptions options(argc, argv);
  int exitCode = 0;
  try {
    exitCode = gctools::startupGarbageCollectorAndSystem(&startup, argc, argv, rl.rlim_max, mpiEnabled, mpiRank, mpiSize);
  } catch (...) {
    return 1; // something is wrong - exiting with non-zero error
  }

#ifdef USE_MPI
  mpip::Mpi_O::Finalize();
#endif
  return exitCode;
}
