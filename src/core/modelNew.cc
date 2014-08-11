#define	DEBUG_LEVEL_NONE

#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "model.h"
#ifdef	USEBOOSTPYTHON
#include <Python.h>
#endif
//#include	"boost/python/module.hpp"
//#include	"boost/python/class.hpp"
#include "boost/utility.hpp"
#include "objectList.h"
#include "wrappers.h"

namespace core {


//#define	DEFINE_SIGNAL_CONSTANTS
//#include	"initSignals.inc"
//#undef	DEFINE_SIGNAL_CONSTANTS


#define	SIGNAL_EXPOSE_CONVERSION_FUNCTIONS
#include "initSignals.inc"
#undef	SIGNAL_EXPOSE_CONVERSION_FUNCTIONS




//
// Constructor
//
    void Model_O::initialize()
{
    this->Base::initialize();
    this->_HoldSignals = false;
}

Model_O::Model_O(const Model_O& mod) : T_O(mod)
{
    this->_HoldSignals = mod._HoldSignals;
    this->_SignalsToHold = mod._SignalsToHold;
}




#if defined(XML_ARCHIVE)
void	Model_O::archiveBase(ArchiveP node)
{ _G();
WeakMultiStringMap<T_O>	tempListeners;
    this->Base::archiveBase(node);
    if ( node->saving() )
    {
	this->cleanupListeners();
	LOG(BF("Saving Model") );
	listenerIterator	it;
	if ( this->_Listeners.size() > 0 )
	{
	    LOG(BF("serializing Model: %s") % this->description().c_str()  );
	    LOG(BF("Serializing(save) %d listeners") % this->_Listeners.size()  );
	    for ( it=this->_Listeners.begin(); it!=this->_Listeners.end(); it++ )
	    {
		tempListeners.insert(signalIdToName(it->first),it->second);
	    }
	    node->archiveWeakMultiStringMapIgnoreBrokenWeakPointers("_listeners",tempListeners);
	    node->setRecognized(true);
	} else
	{
	    LOG(BF("There are NO listeners for Model: %s") % this->description().c_str() );
	}
    } else
    {
	LOG(BF("Loading Model") );
	WeakMultiStringMap<T_O>::iterator	wmmi;
	node->archiveWeakMultiStringMapIfDefined("_listeners",tempListeners);
	this->_Listeners.clear();
	if ( tempListeners.size() > 0 )
	{
	    for (wmmi=tempListeners.begin(); wmmi!=tempListeners.end(); wmmi++ )
	    {
		this->_Listeners.insert(pair<uint,T_wp>(signalNameToId(wmmi->first),wmmi->second));
	    }
	}
    }
}
#endif // defined(XML_ARCHIVE)


bool Model_O::loadFinalize(::core::Dumb_Node* node)
{
    this->cleanupListeners();
    return true;
}

void	Model_O::holdSignals()
{_G();
    ASSERT(!this->_HoldSignals);
    this->_HoldSignals = true;
    this->_SignalsToHold.clear();
}


void	Model_O::releaseSignals()
{_G();
holdIterator	it;
    ASSERT(this->_HoldSignals);
    this->_HoldSignals = false;
    for ( it=this->_SignalsToHold.begin(); it!=this->_SignalsToHold.end(); it++ )
    {
        this->notify(*it);
    }
    this->_SignalsToHold.clear();
}




void	Model_O::connectListener( T_sp listener, Symbol_sp signal )
{_G();
T_sp	realListener;
    realListener = listener->sharedThis<T_O>();
    LOG(BF("Model %s is adding %s to its list of watchers for signal(%s)") % this->description().c_str() % listener->description().c_str() % signalIdToName(signal).c_str()  );
    	// Make sure a listener/signal pair is only added once
    if ( this->recognizesListenerAndSignal(realListener,signal) )
    {
	return;
#if 0
		// I used to just return
	stringstream ss;
	ss << realListener->description();
	ss << " is already listening for signal(" << signalIdToName(signal).c_str();
	ss << ") from " << this->description();
	SIMPLE_ERROR(BF(ss.str()));
#endif
    }
    LOG(BF("Adding listener: %s") % realListener->description().c_str()  );
    this->_Listeners.insert(pair<uint,T_wp>(signal,realListener));
};


bool	Model_O::recognizesListener(T_sp listener)
{_G();
listenerIterator	it;
T_sp				match;
    for ( it=this->_Listeners.begin(); it!=this->_Listeners.end(); it++ )
    {
        if ( NotUndefined(it->second) && it->second.lock().notnilp() )
	{
	    match = it->second.lock();
	    if ( match == listener )
	    {
	        return true;
	    }
	}
    }
    return false;
}


bool	Model_O::recognizesListenerAndSignal(T_sp listener, Symbol_sp signal)
{_G();
listenerIterator	it;
T_sp				match;
    for ( it=this->_Listeners.lower_bound(signal); it!=this->_Listeners.upper_bound(signal); it++ )
    {
	if ( IsUndefined(it->second) ) continue;
        if ( it->second.lock().notnilp() )
	{
	    match = it->second.lock();
	    if ( match == listener )
	    {
	        return true;
	    }
	}
    }
    return false;
}





#ifdef	USEBOOSTPYTHON
void	Model_O::connectPythonObject( Symbol_sp signal, PyObject* object, const string& method, const string& comment )
{_G();
PythonObjectMethod	pyObjMeth;
    Py_INCREF(object);	// Reference count object
    pyObjMeth._Object = object;
    pyObjMeth._Method = method;
    pyObjMeth._Comment = comment;
    this->_PythonObjects.insert(pair<uint,PythonObjectMethod>(signal,pyObjMeth));
}
#endif



void	Model_O::disconnect( T_sp listener, Symbol_sp signal )
{_G();
listenerIterator	it;
T_sp				match;
bool				gotIt = false;
    for ( it=this->_Listeners.lower_bound(signal); it!=this->_Listeners.upper_bound(signal); it++ )
    {
	if ( it->second.lock().notnilp() )
	{
	    match = it->second.lock();
	    if ( match == listener )
	    {
		gotIt = true;
		break;
	    }
	}
    }
    if ( gotIt )
    {
	this->_Listeners.erase(it);
    }
};



void	Model_O::disconnectAll( T_sp listener )
{_G();
T_sp					match;
listenerIterator	it;
vector<listenerIterator>	toss;
vector<listenerIterator>::iterator	tt;
    for ( it=this->_Listeners.begin(); it!=this->_Listeners.end(); it++ )
    {
        if ( it->second.lock().notnilp() )
	{
	    match = it->second.lock();
	    if ( match == listener )
	    {
	        toss.push_back(it);
	    }
	}
    }
    for ( tt=toss.begin(); tt!=toss.end(); tt++ )
    {
	this->_Listeners.erase(*tt);
    }
}


ObjectT_sp	Model_O::listOfListenersForSignal(Symbol_sp signal)
{_G();
listenerIterator	it;
T_sp			obj;
ObjectT_sp			res;
    res = ObjectList_O::create();
    for ( it=this->_Listeners.lower_bound(signal); it!=this->_Listeners.upper_bound(signal); it++ )
    {
	if ( it->second.lock().notnilp())
	{
	    obj = it->second.lock();
	    res->append(obj);
	}
    }
    return res;
}


void	Model_O::cleanupListeners()
{_G();
listenerIterator	it;
vector<listenerIterator>	toss;
vector<listenerIterator>::iterator	tt;
    for ( it=this->_Listeners.begin(); it!=this->_Listeners.end(); it++ )
    {
	if ( IsUndefined(it->second) || it->second.lock().nilp() )
	{
	    toss.push_back(it);
	}
    }
    for ( tt=toss.begin(); tt!=toss.end(); tt++ )
    {
	this->_Listeners.erase(*tt);
    }
}




void	Model_O::_signal(const string& signalType, Symbol_sp sig)
{_G();
#if 0
listenerIterator	it;
T_sp				target;
uint				count;
#ifdef	DEBUG_SIGNALS
const char* _SaveCallerFileName = ::core::debugLog().getTraceFile();
uint	_SaveCallerLineNumber = ::core::debugLog().getTraceLine();
#endif
    if ( this->_HoldSignals )
    {
        LOG(BF("Holding %s: %s") % signalType.c_str() % signalIdToName(sig).c_str()  );
        this->_SignalsToHold.insert(sig);
	return;
    }

#ifdef	DEBUG_SIGNALS
		// Copy the saved sourceFile/lineNumber info
		// so that we dont get confused by internal logging commands
//    ::core::debugLog().set Trace FileLine(_SaveCallerFileName,_SaveCallerLineNumber);
    VP_SIGNAL(("%s(%s) from %s",signalType.c_str(),signalIdToName(sig).c_str(),this->description().c_str() ));
#endif
#ifdef	DEBUG_ON
    LOG(BF("There are %d c++ listeners total -- list follows") % this->_Listeners.size()  );
    listenerIterator	ztt;
    uint					zzi;
    for ( ztt=this->_Listeners.begin(),zzi=0; ztt!=this->_Listeners.end(); ztt++,zzi++ )
    {
	if ( ztt->second.lock().notnilp() )
	{
	    T_sp zmod;
	    zmod = ztt->second.lock();
	    LOG(BF(" Listener #%03d signal(%-20s) %s") % zzi % signalIdToName(ztt->first).c_str() % (zmod)->description().c_str()  );
	} else
	{
	    LOG(BF(" Listener #%03d signal(%-20s) %s") % zzi % signalIdToName(ztt->first).c_str() % "---WeakPointer-NULL---"  );
	}
    }
    LOG(BF("---- list done----") );
#endif
    count = 0;
    for ( it=this->_Listeners.lower_bound(sig); it!=this->_Listeners.upper_bound(sig); it++ )
    {
	if ( (it->second).lock().notnilp() )
	{
	    target = (it->second).lock();
#ifdef DEBUG_SIGNALS
		// Copy the saved sourceFile/lineNumber info
		// so that we dont get confused by internal logging commands
//    ::core::debugLog().set Trace FileLine(_SaveCallerFileName,_SaveCallerLineNumber);
    VP_SIGNAL(("%s(%s) about to be caught by %s",signalType.c_str(),signalIdToName(sig).c_str(),target->description().c_str() ));
#endif
	    LOG(BF("Sending signal to listener: %s") % target->description().c_str()  );
	    target->catchSignal(sig,this->sharedThis<Model_O>());
	    target->propagateSignal(sig);
	    count++;
	} else
	{
	    LOG(BF("There was a listener that was NULL") );
	}
    }
    LOG(BF("There were %d listeners that got signals") % count );
#ifdef	USEBOOSTPYTHON
    PyObject* obj;
    PyTypeObject* objType;
    	// Send the python signals
	//
#ifdef	DEBUG_ON //[
    LOG(BF("There are %d PythonObjects listening for signals") % this->_PythonObjects.size() );
    multimap<uint,PythonObjectMethod>::iterator	zpi;
    for ( zpi=this->_PythonObjects.begin();zpi!=this->_PythonObjects.end();zpi++)
    {
	obj = zpi->second._Object;
	objType = obj->ob_type;
        LOG(BF("   Python watching for signal(%s)  object(%s) method(%s)") % signalIdToName(zpi->first).c_str() % objType->tp_name % zpi->second._Method.c_str() );
    }
    LOG(BF(" ----- python object list done") );
#endif //]
    multimap<uint,PythonObjectMethod>::iterator	pi;
    for ( pi=this->_PythonObjects.lower_bound(sig);pi!=this->_PythonObjects.upper_bound(sig);pi++)
    {
        obj = pi->second._Object;
	objType = obj->ob_type;
#ifdef DEBUG_SIGNALS
		// Copy the saved sourceFile/lineNumber info
		// so that we dont get confused by internal logging commands
	::core::debugLog().setDebugSourceFileName(_SaveCallerFileName.c_str());
	::core::debugLog().DebugLineNumber = _SaveCallerLineNumber;
    VP_SIGNAL(("%s(%s) about to be caught by Python %s:%s",signalType.c_str(),signalIdToName(sig).c_str(),pi->second._Comment.c_str(),pi->second._Method.c_str() ));
#endif
        boost::python::call_method<void,uint,Model_sp>(obj,pi->second._Method.c_str(),sig,this->sharedThis<Model_O>());
    }
#endif
#endif
};

void	Model_O::signal(Symbol_sp sig)
{_G();
    this->_signal("signal",sig);
}


void	Model_O::propagate(Symbol_sp sig)
{_G();
    this->_signal("propagate",sig);
}


class	Model_Expose : public Exposer
{
    void exposeCando()
    {
	class_<Model_O>();

    }
    void exposePython()
    {
#ifdef	USEBOOSTPYTHON //[
    boost::python::def("signalIdToName",&signalIdToName);
    boost::python::def("signalNameToId",&signalNameToId);
    { // Don't mess with this include
        boost::python::enum_<uint>("Signal")
#define	SIGNAL_EXPOSE_TO_BOOST_PYTHON
#include "initSignals.inc"
#undef	SIGNAL_EXPOSE_TO_BOOST_PYTHON
	;
    }


    boost::python::class_<Model_O,
			  gctools::smart_ptr<Model_O>,
	    boost::python::bases <T_O>,
	boost::noncopyable> ("Model_O", boost::python::no_init )
	.def("connectPythonListener",&Model_O::connectPythonObject)
    ;
#endif //]
    }
};



EXPOSE_CLASS(core,Model_O);

};



