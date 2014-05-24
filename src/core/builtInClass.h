#ifndef	_core_BuiltInClass_O_H //[
#define _core_BuiltInClass_O_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "holder.h"

namespace core {


SMART(BuiltInClass );
class BuiltInClass_O : public Class_O
{
    LISP_META_CLASS(StandardClass);
    LISP_BASE1(Class_O);
    LISP_CLASS(core,ClPkg,BuiltInClass_O,"BuiltInClass");
private:
//	string			_InitializationArgumentString;
//	LambdaListHandler_sp	_InitializationArguments;
public:
#if defined(XML_ARCHIVE)
	void	archive(ArchiveP node);
#endif // defined(XML_ARCHIVE)
	void	initialize();
public:
	/*! The normal BuiltInClass creator used once the Lisp environment has been bootstrapped */
    static BuiltInClass_sp create(Symbol_sp instanceClassSymbol);
    /*! Create a BuiltInClass_sp that will always be considered a root object */
    static BuiltInClass_sp createUncollectable();
public:

    virtual void describe();

    /*! Allocate and initialize an instance of this class
     */
    T_sp allocateAndInitialize();
    explicit BuiltInClass_O();
    virtual ~BuiltInClass_O();
};



};
TRANSLATE(core::BuiltInClass_O);



#endif //]
