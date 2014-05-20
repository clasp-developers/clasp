#define	DEBUG_LEVEL_FULL

#define USE_MPI


#include "foundation.h"
#include "object.h"
#include "candoMpi.h"
#include "archiveNode.h"
#include "archive.h"
#include "lisp.h"
#ifdef	USE_MPI
#include <boost/mpi.hpp>
#endif
#include "cons.h"
#include "xmlSaveArchive.h"
#include "xmlLoadArchive.h"
#include "environment.h"
#include "wrappers.h"



/*
__BEGIN_DOC( mpi, chapter, MPI Message Passing core::Interface )

Cando contains a subset of MPI functionality to allow {\CANDOSCRIPT} programs to be written that run in parallel on multiple computers.

Complex {\CANDOSCRIPT} objects are sent back and forth between processes using {\CANDOSCRIPT}s powerful XML serialization code.

In this example code worker processes create Rectangle objects and send them back to a manager process which prints them on the screen.

You can run it by saving it in the file mpi.csc and running: mpirun -np 5 candoMpi mpi.csc

\begin{verbatim}
#
# declare a Rectangle class that stores a width and height
#
[defClass Rectangle [ _width _height ] ]
[defcore::Method init Rectangle [ self w h ] [block
    ( _width = w )
    ( _height = h )
] ]
[defcore::Method repr Rectangle [self] [
    return ("Rectangle(w:%lf h:%lf)" % _width _height ) 
] ]
[defcore::Method area Rectangle [self] [ return ( _width * _height ) ] ]

#
# Get the mpiCommWorld object, the number of worker processes and
# the rank of our process
#
( mpi := [mpiCommWorld] )
( workers := ( [ GetSize mpi ] - 1 ) )
( rank := [ GetRank mpi ] )


[if [ mpiEnabled ] [block 
    [println [format "Process with MPI rank %d started" rank ] ]
#
# If rank == 0 then we are the manager process, 
# wait for messages from the workers.
#
    [if ( rank == 0 ) [block
        # Manager process
    ( cnt := 0 )
    [println ("I'm the manager, waiting for %d objects" % workers )]
    [while (cnt < workers)
        [block
        ( msg := [Recv mpi MPI::ANY_SOURCE MPI::ANY_TAG ] )
        ( source := [ GetSource mpi ] )
        [println ( "Master received message from source: %d" % source ) ]
        [println ( "      Received Rectangle: %s" % ( msg repr ) ) ]
        (cnt := (cnt + 1))
        ]
    ]
    [println "Master done" ]
    ] [ block
# 
# We are a worker process, seed the random number generator with 
# our rank so that every worker starts with different random numbers
# generate a rectangle and send it back to the Manager
#
    [println "Seeding random number generator" ]
    [ seedRandomcore::NumberGenerators [ mpiRank ] ]
    ( rect := [new Rectangle] )
    ( rect init ([randomcore::Number01] * 100.0) ([randomcore::Number01] * 100.0))
    [println ( "Worker rank: %d Rectangle: %s" % [mpiRank] (rect repr) ) ]
    [Send mpi rect 0 0]
    ] ]
] [block
    [println "Not running mpi" ]
] ]
\end{verbatim}



__END_DOC
*/









namespace mpi
{


    STATIC_PREDEFINED_SYMBOL(_sym_MpiTermConverter);


#ifdef	USE_MPI
    static	boost::mpi::environment*	_MpiEnvironment;
#endif
    static	bool				_MpiInitialized = false;
    static	bool				_MpiWorldInitialized = false;
    static	Mpi_sp				_MpiWorld;





/*
  __BEGIN_DOC( mpi.commands, section, MPI Functions)

  A list of MPI functions.
  __END_DOC
*/

/*
  __BEGIN_DOC( mpi.commands.mpiEnabled, subsection, mpiEnabled)
  \scriptCmdRet{mpiEnabled}{}{core::Bool::}

  Return true if MPI is enabled and false if it is not. This function is available in all implementations of {\CANDOSCRIPT}.
  __END_DOC
*/

    bool Mpi_O::mpiEnabled(core::Lisp_sp lisp)
    {_G();
#ifdef	USE_MPI
	return true;
#else
	return false;
#endif
    }


/*
  __BEGIN_DOC( mpi.commands.mpiSize, subsection, mpiSize)
  \scriptCmdRet{mpiSize}{}{core::Int::}

  Returns the number of processes available.  This function is available in all implementations of {\CANDOSCRIPT} but if MPI is not enabled it returns 1.
  __END_DOC
*/
    int Mpi_O::mpiSize(core::Lisp_sp lisp)
    {_G();
#ifdef	USE_MPI
	return mpiCommWorld(lisp)->Get_size();
#else
	return 1;
#endif
    }



/*
  __BEGIN_DOC( mpi.commands.mpiRank, subsection, mpiRank)
  \scriptCmdRet{mpiRank}{}{core::Int::}

  Returns the rank of the current processes. The rank is a number from 0 to ([mpiSize] - 1). This function is available in all implementations of {\CANDOSCRIPT} but if MPI is not enabled it returns 0.
  __END_DOC
*/
    int Mpi_O::mpiRank(core::Lisp_sp env)
    {
#ifdef	USE_MPI
	return mpiCommWorld(env)->Get_rank();
#else
	return 0;
#endif
    }



    void Mpi_O::Init( int& argc, char** &argv, bool& mpiEnabled, int& rank, int& msize)
    {
	mpiEnabled = false;
	rank = -1;
	msize = -1;
	// Do not touch the debug log in this function
#ifdef	USE_MPI
	_MpiEnvironment = new boost::mpi::environment(argc,argv);
	HARD_ASSERTF(_MpiEnvironment,BF("Could not create MPI environment although mpi should be enabled"));
	_MpiInitialized = _MpiEnvironment->initialized();
	if (_MpiInitialized)
	{
	    boost::mpi::communicator world;
	    mpiEnabled = true;
	    rank = world.rank();
	    msize = world.size();
	} else
	{
	    printf("%s %d Could not initialize mpi - exiting\n", __FILE__, __LINE__);
	    exit(1);
	}
#else
	_MpiInitialized = false;
#endif
    }


/*
  __BEGIN_DOC( mpi.commands.mpiCommWorld, subsection, mpiCommWorld)
  \scriptCmdRet{mpiCommWorld}{}{Mpi::}

  Returns the MPI object that encompases the entire group of processes.
  __END_DOC
*/
    Mpi_sp Mpi_O::mpiCommWorld(core::Lisp_sp lisp)
    {_G();
	if ( !_MpiWorldInitialized )
	{
	    _MpiWorldInitialized = true;
	    LOG(BF("_MpiWorld creating") ); // vp0(( "_MpiWorld creating" ));
	    _MpiWorld = Mpi_O::create(lisp);
	    ASSERT_NOT_NULL(_MpiWorld);
	    LOG(BF("status") ); // vp0(("status" ));
	    ASSERT(_MpiWorld->notNil());
	    LOG(BF("status") ); // vp0(("status" ));
	}
	ASSERT_NOT_NULL(_MpiWorld);
	ASSERTP(_MpiWorld->notNil(),"_MpiWorld is nil" );
	return _MpiWorld;
    }



    void Mpi_O::Finalize(const core::Lisp_sp& lisp)
    {_G();
#ifdef	USE_MPI
//    MPI_Finalize();
#endif
    }

/*
  __BEGIN_DOC(mpi.MpiObject,section,MPI Object methods)

  core::Methods that can be sent to MPI objects.
  __END_DOC
*/


    void	Mpi_O::initialize()
    {
	this->Base::initialize();
	this->_Source = 0;
	this->_Tag = 0;
    }





    int	Mpi_O::Get_size()
    {_G();
#ifdef	USE_MPI
	int size;
	size = this->_Communicator.size();
	return size;
#else
	return 1;
#endif
    }

    int	Mpi_O::Get_rank()
    {
#ifdef	USE_MPI
	int rank;
	rank = this->_Communicator.rank();
	return rank;
#else
	return 0;
#endif
    }



// Object_sp obj, int dest, int tag )
/*
  __BEGIN_DOC( mpi.MpiObject.Send, subsection, Send)
  \scriptcore::Method{mpi}{Send}{Object::data core::Int::dest core::Int::tag}

  Sends the \sa{Object::data} to the process \sa{dest} with the tag \sa{tag}. The data can be any Cando-Script object - it is archived into XML format and then sent to the process \sa{dest} and then dearchived back into a Cando-Script object on the other side.
  __END_DOC
*/
    Object_sp Mpi_O::prim_Send(core::Executable_sp e, core::Cons_sp args, core::Environment_sp environ, core::Lisp_sp lisp)
    {_G();
#ifdef	USE_MPI
	Object_sp obj = args->listref<Object_O>(1);
	int dest = args->listref<core::Fixnum_O>(2)->get();
	int tag = args->listref<core::Fixnum_O>(3)->get();
	core::XmlSaveArchive_sp archive = core::XmlSaveArchive_O::create(this->lisp());
	archive->put("only",obj);
	LOG(BF("About to call MPI_Send\n%s\n") % archive->asString() );
	this->_Communicator.send(dest, tag, archive->asString() );
#endif
	return Object_O::nil(this->lisp());
    }


/*
  __BEGIN_DOC( mpi.MpiObject.Recv, subsection, Recv)
  \scriptMethodRet{mpi}{Recv}{core::Int::dest core::Int::tag}{Object::data}

  Blocks and waits for data from the process \sa{dest} with the requested tag \sa{tag}. You can provide the value MPI::ANY\_SOURCE if you want to receive data from any source and MPI::ANY\_TAG if you want any tag. You can use the \scmd{GetSource} and \scmd{GetTag} to query the source and tag that the sender sent. The data that is received is returned in \sa{Object::data}.
  __END_DOC
*/
// Object_sp Mpi_O::Recv(int source, int tag )
    Object_sp Mpi_O::prim_Recv(core::Executable_sp e, core::Cons_sp args, core::Environment_sp environ, core::Lisp_sp lisp )
    {_G();
#ifdef	USE_MPI
	int source = args->listref<core::Fixnum_O>(1)->get();
	int tag = args->listref<core::Fixnum_O>(2)->get();
        LOG(BF("About to call MPI_Probe") ); // vp0(("About to call MPI_Probe"));
	boost::mpi::status stat = this->_Communicator.probe(source,tag);
	this->_Source= stat.source();
	this->_Tag= stat.tag();
	LOG(BF("Probe command returned source %d") % this->_Source  ); // vp0(("Probe command returned source %d", this->_Source ));
#if 0 // boost mpi may not need to allocate a buffer
	int count;
	LOG(BF("About to call MPI_Get_count") ); // vp0(("About to call MPI_Get_count"));
	count = status.count<char>();
	LOG(BF("About to Recv %d characters") % count ); // vp0(("About to Recv %d characters", count));
	char* buffer = (char*)malloc(count*sizeof(char)+10);
	ASSERTP(buffer!=NULL,"the buffer is NULL!");
	LOG(BF("About to call MPI_Recv") ); // vp0(("About to call MPI_Recv"));

	MPI_Recv(buffer,count,MPI_CHAR,
		 this->_Source, this->_Tag,
		 this->_World,&status);
	LOG(BF("Recieved data through mpi data =\n%s\n") % buffer );
	LOG(BF("About to call MPI_Get_count#2") ); // vp0(("About to call MPI_Get_count#2"));
	MPI_Get_count(&status,MPI_CHAR,&count);
	LOG(BF("Received %d characters") % count  ); // vp0(("Received %d characters", count ));
	buffer[count] = '\0';
#endif
	string buffer;
	this->_Communicator.recv(source,tag,buffer);
	core::XmlLoadArchive_sp archive = core::XmlLoadArchive_O::create(lisp);
	archive->parseFromString(buffer);
	Object_sp obj = archive->get("only");
//    free(buffer);
	return obj;
#else
	return Object_O::nil(this->lisp());
#endif
    }


/*
  __BEGIN_DOC( mpi.MpiObject.GetSource, subsection, GetSource)
  \scriptMethodRet{mpi}{GetSource}{}{core::Int::source}

  Returns the source for the most recent Recv command.
  __END_DOC
*/
    int Mpi_O::Get_source()
    {
	return this->_Source;
    }


/*
  __BEGIN_DOC( mpi.MpiObject.GetTag, subsection, GetTag)
  \scriptMethodRet{mpi}{GetTag}{}{core::Int::tag}

  Returns the tag for the most recent Recv command.
  __END_DOC
*/
    int Mpi_O::Get_tag()
    {
	return this->_Tag;
    }


    void Mpi_O::lisp_initGlobals(core::Lisp_sp lisp)
    {
	core::Symbol_sp anySource = lisp->internAndExport(MpiPkg,"ANY_SOURCE");
	anySource->setUniqueDynamicValue(lisp->create<core::Fixnum_O>(boost::mpi::any_source));
	core::Symbol_sp anyTag = lisp->internAndExport(MpiPkg,"ANY_TAG");
	anyTag->setUniqueDynamicValue(lisp->create<core::Fixnum_O>(boost::mpi::any_tag));
    }

    void Mpi_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	LOG(BF("Exposing Mpi_O") ); // vp0(("Exposing Mpi_O"));
	defInPackage(MpiPkg,"mpiEnabled",&Mpi_O::mpiEnabled,lisp);
	defInPackage(MpiPkg,"mpiCommWorld",&Mpi_O::mpiCommWorld,lisp);
	core::class_<Mpi_O>()
	    .def("GetSize",&Mpi_O::Get_size)
	    .def("GetRank",&Mpi_O::Get_rank)
	    .def("GetSource",&Mpi_O::Get_source)
	    .def("GetTag",&Mpi_O::Get_tag)
	    .def_raw("Send",&Mpi_O::prim_Send)
	    .def_raw("Recv",&Mpi_O::prim_Recv)
	    ;
    }

void exposePython()
{
#ifdef	USEBOOSTPYTHON //[
    boost::python::class_<Mpi_O,
	boost::shared_ptr<Mpi_O>,
	boost::python::bases <Object_O>,
	boost::noncopyable> ("Mpi_O", boost::python::no_init )
    ;
#endif
}



EXPOSE_CLASS_AND_GLOBALS(Mpi_O);
};



