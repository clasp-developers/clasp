#define	DEBUG_LEVEL_FULL

#ifdef _TARGET_OS_LINUX
# include <signal.h>
#endif

#ifdef USE_MPI
#include <boost/mpi.hpp>
#endif
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



int startup(int argc, char* argv[], bool& mpiEnabled, int& mpiRank, int& mpiSize )
{
    core::LispHolder lispHolder(mpiEnabled,mpiRank,mpiSize);
    int exitCode = 0;
    try
    {
	lispHolder.startup(argc, argv, "CLASP"); // was "CANDO_APP"

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
    } catch (...) { exitCode = gctools::handleFatalCondition(); }
    return exitCode;
}





int main(int argc, char* argv[] )
{	// Do not touch debug log until after MPI init
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
    int exitCode = gctools::startupGarbageCollectorAndSystem(&startup,argc,argv,mpiEnabled,mpiRank,mpiSize);

#ifdef USE_MPI
    mpip::Mpi_O::Finalize();
#endif
    return exitCode;
}
