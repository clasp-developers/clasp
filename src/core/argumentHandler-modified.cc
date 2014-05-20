#define	DEBUG_LEVEL_FULL

#include "lisp.h"
#include "lambdaListHandler.h"
#include "environment.h"
#include "symbol.h"
#include "reader.h"
#include "package.h"
#include "evaluator.h"
#include "keyedObject.h"

// last include is wrappers.h
#include "wrappers.h"


namespace core {


void LambdaListHandler_O::exposeCando(Lisp_sp e)
{
    class_<LambdaListHandler_O>()
    ;
}

void LambdaListHandler_O::exposePython()
{
#ifdef	USEBOOSTPYTHON //[
    PYTHON_CLASS(CorePkg,LambdaListHandler,"","",_lisp)
	;
#endif //]
}




    LambdaListHandler_sp LambdaListHandler_O::create( const string& argumentsInString, Lisp_sp lisp)
{_G();
    LOG(BF("LambdaListHandler_O::create called with current package[%s] arguments[%s]") % lisp->getCurrentPackage()->getName() % argumentsInString );
    if ( argumentsInString == "" )
    {
	return((_Nil<LambdaListHandler_O>()));
    }
    Reader_sp reader = Reader_O::createOnString(argumentsInString,lisp);
    Cons_sp sscons = reader->read().as_or_nil<Cons_O>();
    reader->close();
    LambdaListHandler_sp ah = lisp->create<LambdaListHandler_O>();
    ah->setupArgumentHandling(sscons);
    ah->setArgumentString(argumentsInString);
    return((ah));
}


void	LambdaListHandler_O::oldLispInitialize(Cons_sp kargs, Lisp_sp env)
{
    this->Base::oldLispInitialize(kargs,env);
    	// your stuff here
}

void	LambdaListHandler_O::initialize()
{
    this->Base::initialize();
}


#if defined(XML_ARCHIVE)
void	LambdaListHandler_O::archiveBase(ArchiveP node)
{
    this->Base::archiveBase(node);
    node->archiveVector0("reqArgs",this->_RequiredArguments);
}
#endif // defined(XML_ARCHIVE)



Environment_sp LambdaListHandler_O::createEnvironmentAndParseArgumentsIntoIt(Cons_sp args, Environment_sp environ)
{_G();
    Environment_sp newEnviron = Environment_O::create(environ,_lisp);
    this->populateLocalEnvironment(args,newEnviron);
    return((newEnviron));
}





string LambdaListHandler_O::requiredArgumentsAsString()
{
    stringstream ss;
    for ( auto ri=this->_RequiredArguments.begin(); ri!=this->_RequiredArguments.end(); ri++ )
    {
	ss << (*ri)->fullName() << " ";
    }
    return((ss.str()));
}



LambdaListHandler_O& LambdaListHandler_O::addRequired(Symbol_sp sym)
{_G();
    if ( this->addArgumentMode() != required )
    {
	SIMPLE_ERROR(BF("You tried to add the required argument(%s) when the LambdaListHandler was in mode(%s)")%sym->fullName()%this->addArgumentModeAsString() );
    }
    this->_RequiredArguments.push_back(sym);
    return((*this));
}






Cons_sp LambdaListHandler_O::populateLocalEnvironmentRequiredArguments(Cons_sp args, Environment_sp environment)
{_OF();
    Cons_sp cur = args;
    {_BLOCK_TRACE("Assigning positional arguments");
        LOG(BF("There are %d positional arguments") % this->_RequiredArguments.size() );
	for ( auto pi = this->_RequiredArguments.begin();
	      pi!=this->_RequiredArguments.end(); pi++ )
	{
	    LOG(BF("Checking if cur.nilp() = %d") % cur.nilp() );
	    if ( cur.nilp() )
	    {
		LOG(BF("Throwing exception because cur isNil"));
		SIMPLE_ERROR(BF("Missing positional argument (%s) - expected in argument list: %s" % (*pi)->fullName() % this->__repr__() ));
	    }
	    T_sp value = oCar(cur);
	    LOG(BF("Updating Binder symbol=%s") % (*pi)->__repr__());
	    LOG(BF("Value it will be assigned: %s") % value->__repr__() );
	    ASSERTNOTNULL(environment);
	    ASSERTP(environment.notnilp(),"The Environmentis nil! - this should never happen");
	    LOG(BF("The environment currently contains: %s") % environment->summaryOfContents() );
	    environment->extend(*pi,value);
	    LOG(BF("Assigned required argument(%s) value(%s)") % (*pi)->__repr__() % oCar(cur)->__repr__() );
	    cur = cCdr(cur);
	}
    }
    return((cur));
}



    EXPOSE_CLASS(core,LambdaListHandler_O);

};




