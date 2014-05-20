#ifndef	_core_standardClass_H
#define _core_standardClass_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "stdClass.h"
#include "holder.h"

namespace core {

SMART(StandardClass);


SMART(StringSet);






SMART(StandardClass );
class StandardClass_O : public StdClass_O
{
    LISP_META_CLASS(StandardClass);
    LISP_BASE1(StdClass_O);
    LISP_CLASS(core,ClPkg,StandardClass_O,"StandardClass");
public:
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
	void	initialize();
protected:
	Class_sp			_InstanceCoreClass;
public:
	/*! Special creator used when starting up lisp environment */
	static StandardClass_sp create(Class_sp mc);

    explicit StandardClass_O();
    virtual ~StandardClass_O() {};
};


class	StandardClassInitializationFunctoid : public Functoid
{
private:
	StandardClass_sp	_StandardClass;	
public:
    virtual string describe() const {return "StandardClassInitializationFunctoid";};
    StandardClassInitializationFunctoid(const string& name, StandardClass_sp c) : Functoid(name)
    { this->_StandardClass = c;};
    virtual ~StandardClassInitializationFunctoid() {};
};

};
TRANSLATE(core::StandardClass_O);
#endif //]
