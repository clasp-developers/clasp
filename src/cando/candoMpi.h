#ifndef	CandoScriptMpi_H //[
#define CandoScriptMpi_H


#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "core/foundation.h"
#include "core/object.h"
#ifdef	USE_MPI
#include <boost/mpi.hpp>
namespace bmpi = boost::mpi;
#endif
#include "mpiPackage.h"

namespace mpi {


    FORWARD(Mpi);
    class Mpi_O : public Object_O
    {
	LISP_BASE1(Object_O);
	LISP_CLASS(MpiPkg,Mpi_O,"Mpi");
	DECLARE_INIT_GLOBALS();
private:
//	bool		_Running;
	int		_LastReturnCode;
	int		_Source;
	int		_Tag;
#ifdef USE_MPI
	boost::mpi::communicator	_Communicator;
#endif
public:
	static void exposeCando(core::Lisp_sp lisp);
	static void initializeGlobals(core::Lisp_sp lisp);
	static void exposePython() {IMPLEMENT_ME();};
public:
   void initialize();

public:
	/*! Equivalent to MPI_Init(argc,argv)
	 * Returns the new commandLineArguments as a list
	 */
   static void Init(int &argc, char** &argv, bool& mpiEnabled, int& rank, int& msize);

    static Mpi_sp mpiCommWorld(core::Lisp_sp);
    	/*! Quick way to get the number of processes
	 * If mpi is not enabled, always returns 1
	 */
    static int mpiSize(core::Lisp_sp);

    	/*! Quick way to get the rank of current process
	 * If mpi is not enabled, always returns 0
	 */
    static int mpiRank(core::Lisp_sp);

    	//! Like MPI_Finalize()
    static void Finalize(const core::Lisp_sp& lisp);
public:

	/*!Return false if mpi is not enabled, true otherwise */
    static bool mpiEnabled(core::Lisp_sp lisp);


	//! Return the sender of the last Recv'd message
    int Get_source();

	//! Return the tag of the last Recv'd message
    int Get_tag();

	//! Return the number of processes communicating through MPI
    int Get_size();
    	//! Return the rank of this process
    int Get_rank();


    	//! Blocking send an object to the destination
    Object_sp prim_Send( core::Executable_sp e,core::Cons_sp args, core::Environment_sp environ, core::Lisp_sp lisp);

	//! Blocking receive an object
    Object_sp prim_Recv( core::Executable_sp e,core::Cons_sp args, core::Environment_sp environ, core::Lisp_sp lisp);



    DEFAULT_CTOR_DTOR(Mpi_O);
    };


};

TRANSLATE(mpi::Mpi_O);
#endif //]
