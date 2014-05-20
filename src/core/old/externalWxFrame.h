#ifndef	ExternalObject_H //[
#define ExternalObject_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"


namespace mbb {


__BEGIN_CLASS_DEFINITION(MbbPackage,O_ExternalObject,ExternalObject,O_Object)
public:
    template <typename T>
        static void expose(T cl)
	{ 
	    cl
	    .def("getMatter",&O_ExternalObject::getMatter)
	    .def("numberOfTrajectoryFrames",&O_ExternalObject::numberOfTrajectoryFrames)
	    .def("addFrame",&O_ExternalObject::addFrame)
	    .def("getTrajectoryFrame",&O_ExternalObject::getTrajectoryFrame)
	    .def("applyTrajectoryFrameToMatter",&O_ExternalObject::applyTrajectoryFrameToMatter)
	    ;
	}
    static void exposeCando(RPLisp e);
    static void exposePython();
public: // virtual functions inherited from Object
	void	initialize();
	void	lispInitialize(RPKeyedArguments kargs, RPLisp);
	void	archiveBase(RPNode node);
//	string	__repr__() const;

private: // instance variables
	string			_ExternalObjectName;
	RPKeyedArguments	_Arguments;

public:
	O_ExternalObject( const O_ExternalObject& ss ); //!< Copy constructor

__END_CLASS_DEFINITION(O_ExternalObject)


};
#endif //]
