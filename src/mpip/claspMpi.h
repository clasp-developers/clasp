#ifndef	BrclScriptMpi_H //[
#define BrclScriptMpi_H


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

namespace mpip {


    FORWARD(Mpi);
    class Mpi_O : public core::T_O
    {
	LISP_BASE1(core::T_O);
	LISP_CLASS(mpip,MpiPkg,Mpi_O,"Mpi");
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
	static void initializeGlobals(core::Lisp_sp lisp);
public:
   void initialize();

public:
	/*! Equivalent to MPI_Init(argc,argv)
	 * Returns the new commandLineArguments as a list
	 */
   static void Init(int &argc, char** &argv, bool& mpiEnabled, int& rank, int& msize);

    static Mpi_sp mpiCommWorld();
    	/*! Quick way to get the number of processes
	 * If mpi is not enabled, always returns 1
	 */
	static int mpiSize();

    	/*! Quick way to get the rank of current process
	 * If mpi is not enabled, always returns 0
	 */
	static int mpiRank();

    	//! Like MPI_Finalize()
	static void Finalize();
public:

	/*!Return false if mpi is not enabled, true otherwise */
    static bool mpiEnabled();


	//! Return the sender of the last Recv'd message
    int Get_source();

	//! Return the tag of the last Recv'd message
    int Get_tag();

	//! Return the number of processes communicating through MPI
    int Get_size();
    	//! Return the rank of this process
    int Get_rank();


    	//! Blocking send an object to the destination
	core::T_sp prim_Send(int dest, int tag, core::T_sp obj);

	/*! Blocking receive an object, return (values object source tag)
	  Alternatively you can recover the source and tag using (get-source (mpi-comm-world))
	  or (get-tag (mpi-comm-world)) */
	
	core::T_mv prim_Recv( int source, int tag );

    DEFAULT_CTOR_DTOR(Mpi_O);
    };


};

TRANSLATE(mpip::Mpi_O);
#endif //]
