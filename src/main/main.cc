#define	DEBUG_LEVEL_FULL

#ifdef _TARGET_OS_LINUX
# include <signal.h>
#endif

#ifdef USE_MPI
#include <boost/mpi.hpp>
#endif
#include <llvm/Support/ErrorHandling.h>
#include <string>
#include "core/foundation.h"
#include "core/bundle.h"
#include "core/object.h"
#include "core/lisp.h"
#include "core/evaluator.h"
#include "core/str.h"
#include "core/symbolTable.h"
#include "core/candoOpenMp.h"
#include "core/cons.h"
#include "core/commandLineOptions.h"
#include "cffi/cffiPackage.h"
#include "llvmo/llvmoPackage.h"
#include "gctools/gctoolsPackage.h"
#include "clbind/clbindPackage.h"
#include "sockets/socketsPackage.h"
#include "serveEvent/serveEventPackage.h"
#include "asttooling/asttoolingPackage.h"
#ifdef USE_MPI
#include "mpip/mpiPackage.h"
#include "mpip/claspMpi.h"
#endif

void handle_signals(int signo) {
    //
    // Indicate that a signal was caught and handle it at a safe-point
//
    SET_SIGNAL(signo);
}


void fatal_error_handler(void* user_data, const std::string& reason, bool gen_crash_diag)
{
    printf("Hit a fatal error in llvm/clang: %s\n", reason.c_str());
    printf("Terminating via exit(0)\n");
    exit(0);
}



#ifdef USE_BOEHM
void clasp_warn_proc(char *msg, GC_word arg)
{
    printf("%s:%d clasp trapped Boehm-gc warning...\n", __FILE__, __LINE__ );
    printf( msg, arg );
}
#endif


int startup(int argc, char* argv[], bool& mpiEnabled, int& mpiRank, int& mpiSize )
{
#if 0 // #ifdef USE_MPI
    try {
	mpip::Mpi_O::Init(argc,argv,mpiEnabled,rank,msize);
    } catch (core::HardError& err) {
	printf("Could not start MPI\n");
	exit(1);
    }
#endif

    core::LispHolder lispHolder(mpiEnabled,mpiRank,mpiSize);
    int exitCode = 0;
    try
    {
	core::TopLevelIHF topFrame(_lisp->invocationHistoryStack(),_Nil<core::T_O>());
	core::_globalProfiler.start();
	lispHolder.startup(argc, argv, "CANDO_APP");
	clbind::ClbindExposer ClbindPkg(_lisp);
	_lisp->installPackage(&ClbindPkg);
	llvmo::LlvmoExposer llvmopkg(_lisp);
	_lisp->installPackage(&llvmopkg);
	cffi::CffiExposer cffipkg(_lisp);
	_lisp->installPackage(&cffipkg);
	gctools::GcToolsExposer GcToolsPkg(_lisp);
	_lisp->installPackage(&GcToolsPkg);
	sockets::SocketsExposer SocketsPkg(_lisp);
	_lisp->installPackage(&SocketsPkg);
	serveEvent::ServeEventExposer ServeEventPkg(_lisp);
	_lisp->installPackage(&ServeEventPkg);
	asttooling::AsttoolingExposer AsttoolingPkg(_lisp);
	_lisp->installPackage(&AsttoolingPkg);
#ifdef USE_MPI
	mpip::MpiExposer TheMpiPkg(_lisp);
	_lisp->installPackage(&TheMpiPkg);
	if ( mpiEnabled ) {
	    core::Symbol_sp mpi = _lisp->internKeyword("MPI-ENABLED");
	    core::Cons_sp features = cl::_sym_STARfeaturesSTAR->symbolValue().as<core::Cons_O>();
	    cl::_sym_STARfeaturesSTAR->defparameter(core::Cons_O::create(mpi,features));
	} else {
	    SIMPLE_ERROR(BF("USE_MPI is true but mpiEnabled is false!!!!"));
	}
#endif	
	_lisp->run();
    } catch ( core::ExitProgram& ee )
    {
// Do nothing
	printf("Caught ExitProgram in %s:%d\n", __FILE__, __LINE__);
	exitCode = ee.getExitResult();
    } catch ( core::TerminateProgramIfBatch& ee )
    {
// Do nothing
	printf("Caught TerminateProgramIfBatch in %s:%d\n", __FILE__, __LINE__);
    } catch (core::Condition& ee)
    {
	IMPLEMENT_MEF(BF("Figure out what to do if we catch a Condition"));
//        printf("Caught Condition at %s:%d - %s\n", __FILE__, __LINE__, ee.message().c_str() );
//        printf("Stack trace:\n%s", ee.conditionObject()->getStackTraceDump().c_str() );
    }
    catch (core::CatchThrow& ee)
    {
	_lisp->print(BF("%s:%d Uncaught THROW frame[%s] - this should NEVER happen - the stack should never be unwound unless there is a CATCH clause that matches the THROW") % __FILE__ % __LINE__ % ee.getFrame() );
    }
    catch (core::HardError& ee)
    {
	_lisp->print(BF("At %s:%d - HardError caught: %s") % __FILE__ % __LINE__ % ee.message() );
    }
#if 0
    catch ( ... )
    {
	_lisp->print(BF("Unknown exception in main - everything should be caught lower down %s:%d") % __FILE__ % __LINE__);
    }
#endif
    _lisp->print(BF("CANDO terminated normally - exiting main\n\n"));

    core::_globalProfiler.end();
    return exitCode;
}





int main(int argc, char* argv[] )
{	// Do not touch debug log until after MPI init

#ifdef USE_BOEHM
    GC_set_all_interior_pointers(1);
    GC_set_warn_proc(clasp_warn_proc);
    GC_init();
#endif

    if (signal(SIGINT, handle_signals) == SIG_ERR) {
	printf("failed to register SIGINT signal-handler with kernel\n");
    }
    if (signal(SIGCHLD, handle_signals) == SIG_ERR) {
	printf("failed to register SIGCHLD signal-handler with kernel\n");
    }
    if (signal(SIGABRT, handle_signals) == SIG_ERR) {
	printf("failed to register SIGABRT signal-handler with kernel\n");
    }
    llvm::install_fatal_error_handler(fatal_error_handler,NULL);


    bool mpiEnabled = false;
    int mpiRank = 0;
    int mpiSize = 1;






#ifdef USE_MPI
    try {
	mpip::Mpi_O::Init(argc,argv,mpiEnabled,mpiRank,mpiSize);
    } catch (core::HardError& err) {
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



    core::CommandLineOptions options(argc,argv);


#if defined(USE_MPS)
    int exitCode = gctools::initializeMemoryPoolSystem(&startup,argc,argv, NULL, mpiEnabled, mpiRank, mpiSize);
#else
    int exitCode = startup(argc,argv,mpiEnabled,mpiRank,mpiSize);
#endif


#ifdef USE_MPI
    mpip::Mpi_O::Finalize();
#endif


    return exitCode;
}
