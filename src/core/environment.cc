#define	DEBUG_LEVEL_FULL

#include <string.h>
#include "core/common.h"
//#i n c l u d e "stringSet.h"
#include "symbolTable.h"
#include "environment.h"
#include "lambdaListHandler.h"
#include "standardObject.h"
#include "multipleValues.h"
#include "core/sequence.h"
#include "hashTableEqual.h"
#include "hashTableEq.h"
#include "activationFrame.h"
#include "evaluator.h"
#include "wrappers.h"


#define	MAX_CONS_CHARS	1024

namespace core
{



    
    
#define ARGS_af_countFunctionContainerEnvironments "(arg)"
#define DECL_af_countFunctionContainerEnvironments ""
#define DOCS_af_countFunctionContainerEnvironments "countFunctionContainerEnvironments"
    T_mv af_countFunctionContainerEnvironments()
    {_G();
	IMPLEMENT_MEF(BF("Implement countFunctionContainerEnvironments"));
    };


    
    
#define ARGS_af_environmentActivationFrame "(env)"
#define DECL_af_environmentActivationFrame ""
#define DOCS_af_environmentActivationFrame "environmentActivationFrame"
    T_sp af_environmentActivationFrame(Environment_sp env)
    {_G();
	if ( env.nilp() ) return env;
	return env->getActivationFrame();
    };


    
    
#define ARGS_af_environmentList "(env)"
#define DECL_af_environmentList ""
#define DOCS_af_environmentList "Return a list of environment parents"
    Sequence_sp af_environmentList(Environment_sp env)
    {_G();
	Cons_sp result = _Nil<Cons_O>();
	for ( Environment_sp ecur=env; ecur.notnilp(); ecur=ecur->getParentEnvironment() )
	{
	    result = Cons_O::create(ecur,result);
	}
	return(af_nreverse(result).as<Sequence_O>());
    };


    
#define ARGS_af_environmentTypeList "(env)"
#define DECL_af_environmentTypeList ""
#define DOCS_af_environmentTypeList "Return a list of environment parents"
    T_sp af_environmentTypeList(Environment_sp env)
    {_G();
	Cons_sp result = _Nil<Cons_O>();
	for ( Environment_sp ecur=env; ecur.notnilp(); ecur=ecur->getParentEnvironment() )
	{
	    result = Cons_O::create(lisp_static_class(ecur),result);
	}
	return af_nreverse(result);
    };



    
    
    int Environment_O::nilCheck_countFunctionContainerEnvironments(Environment_sp env)
    {_G();
	if ( env.nilp() ) return 0;
	return env->countFunctionContainerEnvironments();
    };


    
    
#define ARGS_af_runtimeEnvironment "(env)"
#define DECL_af_runtimeEnvironment ""
#define DOCS_af_runtimeEnvironment "Return the RuntimeEnvironment or nil"
    T_mv af_runtimeEnvironment(T_sp tenv)
    {_G();
	if ( tenv.nilp() ) return(Values(_Nil<Environment_O>()));
	Environment_sp env = tenv.as_or_nil<Environment_O>();
	return(Values(env->runtimeEnvironment()));
    };


    
    
    
    
#define ARGS_af_environmentId "(env)"
#define DECL_af_environmentId ""
#define DOCS_af_environmentId "environmentId"
    int af_environmentId(Environment_sp env)
    {_G();
	if ( env.nilp() )
	{
	    return 0;	
	}
	return env->environmentId();
    };



    void Environment_O::setRuntimeEnvironment(T_sp renv)
    {_G();
	SIMPLE_ERROR(BF("Only RuntimeVisibleEnvironments support runtime environments"));
    }

    T_sp Environment_O::runtimeEnvironment() const
    {_G();
	SIMPLE_ERROR(BF("Only RuntimeVisibleEnvironments support runtime environments"));
    }

    Environment_sp Environment_O::getParentEnvironment() const
    {
	SUBIMP();
    }


    ActivationFrame_sp Environment_O::nilCheck_getActivationFrame(Environment_sp tenv)
    {_G();
	if ( tenv.nilp() ) return(_Nil<ActivationFrame_O>());
	return(tenv->getActivationFrame());
    };


    ActivationFrame_sp Environment_O::getActivationFrame() const
    {
	SUBCLASS_MUST_IMPLEMENT();
    }


    EXPOSE_CLASS(core,Environment_O);

    void Environment_O::exposeCando(Lisp_sp lisp)
    {
	class_<Environment_O>()
	    .def("setRuntimeEnvironment",&Environment_O::setRuntimeEnvironment)
	    .def("classifyValue",&Environment_O::classifyValue)
	    .def("classifyFunctionLookup",&Environment_O::classifyFunctionLookup)
	    .def("getParentEnvironment",&Environment_O::getParentEnvironment)
	    .def("setf_metadata",&Environment_O::setf_metadata)
	    .def("push_metadata",&Environment_O::push_metadata)
	    .def("localMetadata",&Environment_O::localMetadata)
	    .def("lookupMetadata",&Environment_O::lookupMetadata)
	    .def("gather_metadata",&Environment_O::gather_metadata)
	    .def("find_tagbody_tag_environment",&Environment_O::find_tagbody_tag_environment)
	    .def("find_block_named_environment",&Environment_O::find_block_named_environment)
	    .def("find_unwindable_environment",&Environment_O::find_unwindable_environment)
	    .def("lexicalEnvironmentP",&Environment_O::lexicalEnvironmentP)
	    .def("unwindProtectEnvironmentP",&Environment_O::unwindProtectEnvironmentP)
	    .def("functionContainerEnvironmentP",&Environment_O::functionContainerEnvironmentP)
	    .def("recognizesBlockSymbol",&Environment_O::recognizesBlockSymbol)
	    .def("getBlockSymbolFrame",&Environment_O::getBlockSymbolFrame)
	    .def("classifyTag",&Environment_O::classifyTag)
	    .def("countFunctionContainerEnvironments",&Environment_O::countFunctionContainerEnvironments)
	    ;
	SYMBOL_SC_(CorePkg,environmentActivationFrame);
	Defun(environmentActivationFrame);
	SYMBOL_SC_(CorePkg,currentVisibleEnvironment);
	af_def(CorePkg,"currentVisibleEnvironment",&Environment_O::nilCheck_currentVisibleEnvironment);
	SYMBOL_SC_(CorePkg,runtimeEnvironment);
	Defun(runtimeEnvironment);
	SYMBOL_SC_(CorePkg,environmentList);
	Defun(environmentList);
	SYMBOL_SC_(CorePkg,environmentTypeList);
	Defun(environmentTypeList);
	SYMBOL_SC_(CorePkg,environmentId);
	Defun(environmentId);
    }

    void Environment_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Environment,"","",_lisp)
	    ;
#endif
    }



//
// Constructor
//




    Environment_sp Environment_O::nilCheck_currentVisibleEnvironment(Environment_sp env)
    {_G();
	if ( env.nilp() ) return(_Nil<Environment_O>());
	return(env->currentVisibleEnvironment());
    };

    Environment_sp Environment_O::currentVisibleEnvironment() const
    {
	SUBIMP();
    };





    void Environment_O::setupParent(Environment_sp environ)
    {_G();
    }


    void Environment_O::nilCheck_environmentStackFill(Environment_sp env, int level, stringstream& sout)
    {
	if ( env.nilp() )
	{
	    sout << "NIL";
	    return;
	}
	env->_environmentStackFill(level,sout);
    }

    void Environment_O::_environmentStackFill(int level, stringstream& sout)
    {_OF();
	sout << this->summaryOfContents();
	if ( this->getParentEnvironment().notnilp() )
	{
	    nilCheck_environmentStackFill(this->getParentEnvironment(),level+1,sout);
	}
    }

    string Environment_O::environmentStackAsString()
    {_OF();
	stringstream sout;
	this->_environmentStackFill(1,sout);
	return sout.str();
    }


    Cons_sp Environment_O::nilCheck_gather_metadata(Environment_sp env, Symbol_sp key)
    {
	if ( env.nilp() ) return _Nil<Cons_O>();
	return env->gather_metadata(key);
    }

    Cons_sp Environment_O::gather_metadata(Symbol_sp key) const
    {_G();
	if ( this->getParentEnvironment().nilp() ) return _Nil<Cons_O>();
	return nilCheck_gather_metadata(this->getParentEnvironment(),key);
    }



    T_mv Environment_O::lookupMetadata(Symbol_sp key) const
    {_G();
	if ( this->getParentEnvironment().nilp())
	{
	    return(Values(_Nil<T_O>(),_Nil<T_O>(),_Nil<Environment_O>()));
	}
	return(this->getParentEnvironment()->lookupMetadata(key));
    }


    T_mv Environment_O::localMetadata(Symbol_sp key) const
    {_G();
	SUBCLASS_MUST_IMPLEMENT();
    }






    T_sp Environment_O::lookupValue(int depth, int index ) const
    {_G();
	if ( depth == 0  )
	{
	    SIMPLE_ERROR(BF("Could not find value"));
	}
	if ( this->getParentEnvironment().nilp() ) 
	{
	    SIMPLE_ERROR(BF("Could not find value"));
	}
	--depth;
	return this->getParentEnvironment()->lookupValue(depth,index);
    }



    string Environment_O::__repr__() const
    {_G();
	stringstream ss;
	int tab = _sym_STARenvironmentPrintingTabSTAR->symbolValue().as<Fixnum_O>()->get();
	{
	    ss << (BF("--------------------------- %20s :id %5d -----") % this->_instanceClass()->classNameAsString() % this->_EnvId ).str() << std::endl;
	    tab += _sym_STARenvironmentPrintingTabIncrementSTAR->symbolValue().as<Fixnum_O>()->get();
	    Fixnum_sp fntab = Fixnum_O::create(tab);
	    DynamicScopeManager scope(_sym_STARenvironmentPrintingTabSTAR,fntab);
	    ss <<this->summaryOfContents();
	    if ( this->getParentEnvironment().notnilp() )
	    {
		ss << string(tab,' ') << " :parent ";
		ss << _rep_(this->getParentEnvironment());
	    }
	    ss << string(tab,' ') << " ]" << std::endl;
	}
	return ss.str();
    }



    bool Environment_O::updateValue(Symbol_sp sym, T_sp obj)
    {_G();
	if ( this->getParentEnvironment().nilp() )
	{
	    SIMPLE_ERROR(BF("Could not update local symbol(%s) because it was not defined") % _rep_(sym) );
	}
	return this->getParentEnvironment()->updateValue(sym,obj);
    }










    bool Environment_O::findValue(Symbol_sp sym, int& depth, int& index, bool& special, T_sp& value) const
    {_G();
	depth = 0;
	index =-1;
	special = false;
	return this->_findValue(sym,depth,index,special,value);
    }


    bool Environment_O::nilCheck_findValue(Environment_sp env, Symbol_sp sym, int& depth, int& index, bool& special,T_sp& value)
    {_G();
	if ( env.nilp() )
	{
	    depth = -1;
	    index = -1;
	    return false;
	}
	return env->_findValue(sym,depth,index,special,value);
    }


    bool Environment_O::_findValue(Symbol_sp sym, int& depth, int& index, bool& special,T_sp& value) const
    {_G();
	Environment_sp parent = nilCheck_currentVisibleEnvironment(this->getParentEnvironment());
	return nilCheck_findValue(parent,sym,depth,index,special,value);
    }


    bool Environment_O::nilCheck_findFunction(Environment_sp env, T_sp functionName, int& depth, int& index, Function_sp& func)
    {
	if ( env.nilp() ) 
	{
	    depth = -1;
	    index = -1;
	    return false;
	}
	return env->_findFunction(functionName,depth,index,func);
    }


    bool Environment_O::_findFunction(T_sp functionName, int& depth, int& index, Function_sp& func) const
    {_G();
	return nilCheck_findFunction(this->getParentEnvironment(),functionName,depth,index,func);
    }


    bool Environment_O::findFunction(T_sp functionName, int& depth, int& index, Function_sp& value) const
    {_G();
	depth = 0;
	index =-1;
	return this->_findFunction(functionName,depth,index,value);
    }



    bool Environment_O::nilCheck_findMacro(Environment_sp env, Symbol_sp sym, int& depth, int& index, Function_sp& func)
    {
	if ( env.nilp() )
	{
	    // Look in the global environment
	    depth = -1;
	    index = -1;
//	    func = sym->symbolFunction();
	    return false;
	}
	return env->_findMacro(sym,depth,index,func);
    }

    bool Environment_O::_findMacro(Symbol_sp sym, int& depth, int& index, Function_sp& func) const
    {_G();
	return nilCheck_findMacro(this->getParentEnvironment(),sym,depth,index,func);
    }


    bool Environment_O::findMacro(Symbol_sp sym, int& depth, int& index, Function_sp& value) const
    {_G();
	depth = 0;
	index =-1;
	return this->_findMacro(sym,depth,index,value);
    }


    bool Environment_O::nilCheck_findSymbolMacro(Environment_sp env, Symbol_sp sym, int& depth, int& index, bool& shadowed, Function_sp& func)
    {_G();
	if ( env.nilp() )
	{
	    depth = -1;
	    index = -1;
	    shadowed = false;
	    return false;
	}
	return env->_findSymbolMacro(sym,depth,index,shadowed,func);
    }

    bool Environment_O::_findSymbolMacro(Symbol_sp sym, int& depth, int& index, bool& shadowed, Function_sp& func) const
    {_G();
	return nilCheck_findSymbolMacro(this->getParentEnvironment(),sym,depth,index,shadowed,func);
    }


    bool Environment_O::findSymbolMacro(Symbol_sp sym, int& depth, int& index, bool& shadowed, Function_sp& value) const
    {_G();
	depth = 0;
	index =-1;
	shadowed = false;
	return this->_findSymbolMacro(sym,depth,index,shadowed,value);
    }



    bool Environment_O::lexicalSpecialP(Symbol_sp sym) const
    {_G();
	if (this->getParentEnvironment().nilp() ) return false;
	return this->getParentEnvironment()->lexicalSpecialP(sym);
    }




    Cons_sp Environment_O::classifyValue(Symbol_sp sym) const
    {_G();
	int depth;
	int index;
	bool special;
	T_sp value;
	if ( this->findValue(sym,depth,index,special,value) )
	{
	    return Cons_O::createList(ext::_sym_lexicalVar,sym,Fixnum_O::create(depth),Fixnum_O::create(index));
	}
	if ( special )
	{
	    return Cons_O::create(ext::_sym_specialVar,sym);
	}
	// Lexical variable was not found - return nil
	return _Nil<Cons_O>();
    }


    Cons_sp Environment_O::classifyTag(Symbol_sp tag)
    {_G();
	int depth;
	int index;
	if (this->findTag(tag,depth,index))
	{
	    return Cons_O::create(_sym_dynamicGo,Cons_O::create(Fixnum_O::create(depth),Fixnum_O::create(index)));
	}
	SIMPLE_ERROR(BF("Could not find tag %s") % _rep_(tag) );

    }


    Cons_sp Environment_O::classifyFunctionLookup(T_sp functionName) const
    {_G();
	int depth;
	int index;
	Function_sp value;
	if ( this->findFunction(functionName,depth,index,value) )
	{
	    return Cons_O::createList(_sym_lexicalFunction,functionName,Fixnum_O::create(depth),Fixnum_O::create(index));
	}
	// If the function was not lexical then it is automatically special
	return Cons_O::create(_sym_globalFunction,functionName);
    }

    Environment_sp Environment_O::find_current_code_environment() const
    {_OF();
	if ( this->getParentEnvironment().nilp() )
	{
	    SIMPLE_ERROR(BF("Could not find current code environment") );
	}
	return this->getParentEnvironment()->find_current_code_environment();
    }



    bool Environment_O::recognizesBlockSymbol(Symbol_sp sym) const
    {_G();
	if ( this->getParentEnvironment().nilp() ) return false;
	return this->getParentEnvironment()->recognizesBlockSymbol(sym);
    }



    int Environment_O::getBlockSymbolFrame(Symbol_sp sym) const
    {_G();
	if ( this->getParentEnvironment().nilp() )
	{
	    SIMPLE_ERROR(BF("Could not find block with name %s") % _rep_(sym));
	}
	return this->getParentEnvironment()->getBlockSymbolFrame(sym);
    }

    bool Environment_O::nilCheck_findTag(Environment_sp env, Symbol_sp sym, int& depth, int& index)
    {
	if ( env.nilp() ) return false;
	return env->_findTag(sym,depth,index);
    }

    bool Environment_O::_findTag(Symbol_sp sym, int& depth, int& index) const
    {_G();
	return nilCheck_findTag(this->getParentEnvironment(),sym,depth,index);
    }


    bool Environment_O::findTag(Symbol_sp sym, int& depth, int& index) const
    {_G();
	depth = 0;
	index = 0;
	return this->_findTag(sym,depth,index);
    }

    int Environment_O::countFunctionContainerEnvironments() const
    {
	return nilCheck_countFunctionContainerEnvironments(this->getParentEnvironment());
    }



    Environment_sp Environment_O::find_block_named_environment(Symbol_sp blockName) const
    {_OF();
	Environment_sp parent = this->getParentEnvironment();
	if ( parent.nilp() )
	{
	    SIMPLE_ERROR(BF("Could not find block with name[%s]") % _rep_(blockName) );
	}
	return parent->find_block_named_environment(blockName);
    }



    Environment_sp Environment_O::find_unwindable_environment() const
    {_OF();
	if ( this->getParentEnvironment().nilp() )
	{
	    return _Nil<Environment_O>();
	}
	return this->getParentEnvironment()->find_unwindable_environment();
    }


    Environment_sp Environment_O::find_tagbody_tag_environment(Symbol_sp tag) const
    {_OF();
	if ( this->getParentEnvironment().nilp() )
	{
	    SIMPLE_ERROR(BF("Could not find tagbody tag with name[%s]") % _rep_(tag) );
	}
	return this->getParentEnvironment()->find_tagbody_tag_environment(tag);
    }



    string Environment_O::nilCheck_summaryOfContents(Environment_sp env)
    {
	int tab = _sym_STARenvironmentPrintingTabSTAR->symbolValue().as<Fixnum_O>()->get();
	stringstream ss;
	if ( env.nilp() )
	{
	    ss << string(tab,' ')<<"#<Environment nil>" << std::endl;
	    return ss.str();
	}
	return env->summaryOfContents();
    }
	

    string Environment_O::summaryOfContents() const
    {_G();
	int tab = _sym_STARenvironmentPrintingTabSTAR->symbolValue().as<Fixnum_O>()->get();
	stringstream ss;
	ss << string(tab,' ')<<"#<Environment_O::-no-contents->" << std::endl;
	return ss.str();
    }





    /*! If the value is lexical return (values val T).
      If the value is locally special return (values T nil).
      If the variable is not found return (values nil nil) */
    T_mv Environment_O::variable_lookup(Symbol_sp sym) const
    {_G();
	int depth, index;
	bool special;
	T_sp value;
	bool found = this->findValue(sym,depth,index,special,value);
	if ( !found )
	{
	    return(Values(_lisp->_boolean(special),_Nil<T_O>()));
	}
	return(Values(value,_lisp->_true()));
    }

    T_mv Environment_O::variable_lookup(const string& package, const string& symStr) const
    {_G();
	// TODO: Ditch this function - we shouldn't lookup symbols like this
	Symbol_sp sym = _lisp->internWithPackageName(package,symStr);
	return this->variable_lookup(sym);
    }



    Function_sp Environment_O::function_lookup(T_sp functionName)
    {_G();
	int depth, index;
	Function_sp func;
	if (this->findFunction(functionName,depth,index,func) )
	{
	    return func;
	}
	return _Nil<Function_O>();
    }




#if 0

    Function_sp Environment_O::lookupSymbolMacro(Symbol_sp sym, bool& foundIt) const
    {_G();
	LOG(BF("Looking to see if there is a symbol-macro with name(%s)") % _rep_(sym) );
	ASSERTNOTNULL(this->getParentEnvironment());
	if ( this->getParentEnvironment().nilp() )
	{
	    // There is no symbol-macro with this name, return nil/false
	    foundIt = false;
	    return lisp()->nil<Function_O>();
	}
	return this->getParentEnvironment()->lookupSymbolMacro(sym,foundIt);
    }






#endif






    LexicalEnvironment_O::LexicalEnvironment_O() : Base() {};


    EXPOSE_CLASS(core,LexicalEnvironment_O);



    void LexicalEnvironment_O::initialize()
    {
	this->Base::initialize();
        this->_Metadata = HashTableEq_O::create_default();
    }

    void LexicalEnvironment_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<LexicalEnvironment_O>()
	    ;
    }

    void LexicalEnvironment_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,LexicalEnvironment,"","",_lisp)
	    ;
#endif
    }

    T_sp LexicalEnvironment_O::setf_metadata(Symbol_sp key, T_sp val)
    {
        this->_Metadata->hash_table_setf_gethash(key,val);
        return val;
    };


    void LexicalEnvironment_O::setupParent(Environment_sp environ)
    {_G();
	this->_ParentEnvironment = environ;
	this->Base::setupParent(environ);
    }

    Environment_sp LexicalEnvironment_O::getParentEnvironment() const
    {_OF();
	ASSERTNOTNULL(this->_ParentEnvironment);
	return this->_ParentEnvironment;
    }

    string LexicalEnvironment_O::summaryOfContents() const
    {_G();
	int tab = _sym_STARenvironmentPrintingTabSTAR->symbolValue().as<Fixnum_O>()->get();
	stringstream ss;
	if ( this->_Metadata->hashTableSize() > 0 )
	{
	    ss << string(tab,' ') << "----Metadata follows ---"<<std::endl;
            this->_Metadata->mapHash( [tab,&ss] (T_sp key, T_sp val) {
                    ss << string(tab,' ')<< _rep_(key) << " --> " << _rep_(val) << std::endl;
		} );
	    ss << string(tab,' ')<< "-----Metadata done ----" <<std::endl;
	} else
	{
	    ss << string(tab,' ') << "----NO METADATA----" << std::endl;
	}
	return ss.str();
    }



    Cons_sp LexicalEnvironment_O::gather_metadata(Symbol_sp key) const
    {_G();
	Cons_sp parentGathered = _Nil<Cons_O>();
	if ( this->getParentEnvironment().notnilp() )
	{
	    parentGathered = nilCheck_gather_metadata(this->getParentEnvironment(),key);
	}
	Cons_sp keyValue = this->_Metadata->find(key);
	if ( keyValue.notnilp() )
	{
	    return Cons_O::create(oCdr(keyValue),parentGathered);
	}
	return parentGathered;
    }



    Cons_sp LexicalEnvironment_O::push_metadata(Symbol_sp key, T_sp val)
    {
	Cons_sp one = Cons_O::create(val,this->localMetadata(key));
	this->_Metadata->hash_table_setf_gethash(key,one);
	return one;
    }

    T_mv LexicalEnvironment_O::localMetadata(Symbol_sp key) const
    {_G();
	Cons_sp it = this->_Metadata->find(key);
	if ( it.nilp() )
	{
	    return(Values(_Nil<T_O>(),_Nil<T_O>()));
	}
	return(Values(oCdr(it),_lisp->_true()));
    }

    T_mv LexicalEnvironment_O::lookupMetadata(Symbol_sp key) const
    {_G();
	Cons_sp it = this->_Metadata->find(key);
	if ( it.nilp() )
	{
	    if ( this->_ParentEnvironment.nilp())
	    {
		return(Values(_Nil<T_O>(),_Nil<T_O>(),_Nil<Environment_O>()));
	    }
	    return this->_ParentEnvironment->lookupMetadata(key);
	}
	return(Values(oCdr(it),_lisp->_true(),this->const_sharedThis<Environment_O>()));
    }








    EXPOSE_CLASS(core,RuntimeVisibleEnvironment_O);

    void RuntimeVisibleEnvironment_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<RuntimeVisibleEnvironment_O>()
	    ;
    }

    void RuntimeVisibleEnvironment_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,RuntimeVisibleEnvironment,"","",_lisp)
	    ;
#endif
    }


    RuntimeVisibleEnvironment_O::RuntimeVisibleEnvironment_O() : Base() {};


    Environment_sp RuntimeVisibleEnvironment_O::currentVisibleEnvironment() const
    {_G();
//	if ( this -> isNil() ) return _Nil<Environment_O>();
	return this->const_sharedThis<Environment_O>();
    }


    bool RuntimeVisibleEnvironment_O::_findTag(Symbol_sp sym, int& depth, int& index ) const
    {_G();
	Environment_sp parent = nilCheck_currentVisibleEnvironment(this->getParentEnvironment());
        ++depth;
	return nilCheck_findTag(parent,sym,depth,index);
    }



    bool RuntimeVisibleEnvironment_O::_findValue(Symbol_sp sym, int& depth, int& index, bool& special,T_sp& value) const
    {_G();
	Environment_sp parent = nilCheck_currentVisibleEnvironment(this->getParentEnvironment());
	if ( parent.nilp() ) return false;
	++depth;
	return nilCheck_findValue(parent,sym,depth,index,special,value);
    }


    bool RuntimeVisibleEnvironment_O::_findFunction(T_sp functionName, int& depth, int& index, Function_sp& func) const
    {
//	if (this -> isNil()) return false;
	Environment_sp parent = nilCheck_currentVisibleEnvironment(this->getParentEnvironment());
	LOG(BF("Moving down a level"));
	++depth;
	return nilCheck_findFunction(parent,functionName,depth,index,func);
    }





    void ValueEnvironment_O::initialize()
    {
        this->Base::initialize();
        this->_SymbolIndex = HashTableEq_O::create_default();
    }


    bool ValueEnvironment_O::lexicalSpecialP(Symbol_sp sym) const
    {_G();
	// Lookup the symbol in our list Symbol map
	Cons_sp fi = this->_SymbolIndex->find(sym);
	if ( fi.nilp() )
	{
	    // if we don't find it then invoke Environment_O::lexicalSpecialP
	    return this->Base::lexicalSpecialP(sym);
	}
	// If the target index is a SPECIAL_TARGET then return true otherwise false
	return (oCdr(fi).as<Fixnum_O>()->get() == SPECIAL_TARGET);
    }


    ActivationFrame_sp ValueEnvironment_O::getActivationFrame() const
    {
//	if ( this -> isNil()) return _Nil<ActivationFrame_O>();
	return this->_ActivationFrame;
    }


    T_sp ValueEnvironment_O::lookupValue(int depth, int index ) const
    {_G();
	if ( depth == 0  )
	{
	    return this->_ActivationFrame->entry(index);
	}
	Environment_sp parent = nilCheck_currentVisibleEnvironment(this->getParentEnvironment());
	if ( parent.nilp() ) 
	{
	    SIMPLE_ERROR(BF("Ran out of parent environments - could not find value"));
	}
	--depth;
	return parent->lookupValue(depth,index);
    }




    ValueEnvironment_O::ValueEnvironment_O() : Base() {};

    ValueEnvironment_O::~ValueEnvironment_O()
    {_G();
    }


    void ValueEnvironment_O::defineLexicalBinding(Symbol_sp sym, int idx)
    {_G();
	Cons_sp it = this->_SymbolIndex->find(sym);
	if ( it.notnilp() )
	{
	    if ( idx != oCdr(it).as<Fixnum_O>()->get() )
	    {
		SIMPLE_ERROR(BF("The lexical variable[%s] is already defined with index[%d] - we tried to set it to[%d]") % _rep_(sym) % oCdr(it) % idx );
	    }
	    return;
	}
        this->_SymbolIndex->hash_table_setf_gethash(sym,Fixnum_O::create(idx));
    }

    void ValueEnvironment_O::defineSpecialBinding(Symbol_sp sym)
    {_G();
	Cons_sp it= this->_SymbolIndex->find(sym);
	if ( it.notnilp() )
	{
	    if ( SPECIAL_TARGET != oCdr(it).as<Fixnum_O>()->get() )
	    {
		SIMPLE_ERROR(BF("The lexical variable[%s] is already defined idx[%s]  - we tried to set it to special") % _rep_(sym) % oCdr(it) );
	    }
	    return;
	}
	this->_SymbolIndex->hash_table_setf_gethash(sym,Fixnum_O::create(SPECIAL_TARGET));
    }






    bool ValueEnvironment_O::_findValue(Symbol_sp sym, int& depth, int& index, bool& special, T_sp& value) const
    {_G();
	LOG(BF("Looking for binding for symbol(%s)") % _rep_(sym) );
//    LOG(BF("The frame stack is %d deep") % this->depth() );
	Cons_sp fi = this->_SymbolIndex->find(sym);
	if ( fi.nilp() )
	{
	    return this->Base::_findValue(sym,depth,index,special,value);
	}
	index = oCdr(fi).as<Fixnum_O>()->get();
	if ( index < 0 )
	{
	    special = true;
	    return false;
	}
	LOG(BF(" Found binding %s")% fi->second );
	value = this->_ActivationFrame->entry(index);
	return true;
    }


    bool ValueEnvironment_O::_findSymbolMacro(Symbol_sp sym, int& depth, int& index, bool& shadowed, Function_sp& fn) const
    {_G();
	LOG(BF("Looking for binding for symbol(%s)") % _rep_(sym) );
//    LOG(BF("The frame stack is %d deep") % this->depth() );
	Cons_sp fi = this->_SymbolIndex->find(sym);
	if ( fi.nilp() )
	{
	    return this->Base::_findSymbolMacro(sym,depth,index,shadowed,fn);
	}
	index = oCdr(fi).as<Fixnum_O>()->get();
	shadowed = true;
	return false;
    }





    bool ValueEnvironment_O::activationFrameElementBoundP(int idx) const
    {_G();
	return this->_ActivationFrame->boundp_entry(idx);
    }



    ValueEnvironment_sp ValueEnvironment_O::createForLambdaListHandler(LambdaListHandler_sp llh, Environment_sp parent)
    {_G();
	ValueEnvironment_sp env(ValueEnvironment_O::create());
	env->setupForLambdaListHandler(llh,parent);
	return env;
    }

    ValueEnvironment_sp ValueEnvironment_O::createForNumberOfEntries(int numberOfArguments, Environment_sp parent)
    {_G();
	ValueEnvironment_sp env(ValueEnvironment_O::create());
	env->setupParent(parent);
	env->_ActivationFrame = ValueFrame_O::create(numberOfArguments,nilCheck_getActivationFrame(nilCheck_currentVisibleEnvironment(parent)));
	return env;
    }


    ValueEnvironment_sp ValueEnvironment_O::createForLocallySpecialEntries(Cons_sp specials, Environment_sp parent)
    {_G();
	ValueEnvironment_sp env(ValueEnvironment_O::create());
	env->setupParent(parent);
	env->_ActivationFrame = ValueFrame_O::create(0,nilCheck_getActivationFrame(nilCheck_currentVisibleEnvironment(parent)));
	for ( Cons_sp cur=specials; cur.notnilp(); cur=cCdr(cur) )
	{
	    env->defineSpecialBinding(oCar(cur).as<Symbol_O>());
	}
	return env;
    }



    void ValueEnvironment_O::setupForLambdaListHandler(LambdaListHandler_sp llh, Environment_sp parent)
    {_G();
	Cons_sp classifiedSymbols = llh->classifiedSymbols();
	this->setupParent(parent);
	int numberOfLexicals = 0;
	for ( Cons_sp cur = classifiedSymbols; cur.notnilp(); cur=cCdr(cur) )
	{
	    Cons_sp classifiedSymbol = oCar(cur).as_or_nil<Cons_O>();
	    Symbol_sp classification = oCar(classifiedSymbol).as<Symbol_O>();
	    if ( classification == ext::_sym_lexicalVar )
	    {
		++numberOfLexicals;
	    } else if ( classification == ext::_sym_specialVar )
	    {
		// handle special declarations
		Symbol_sp sym = oCdr(classifiedSymbol).as<Symbol_O>();
		this->defineSpecialBinding(sym);
	    }
	}
	this->_ActivationFrame = ValueFrame_O::create(numberOfLexicals,nilCheck_getActivationFrame(nilCheck_currentVisibleEnvironment(parent)));
    }


    string	ValueEnvironment_O::summaryOfContents() const
    {_G();
	int tab = _sym_STARenvironmentPrintingTabSTAR->symbolValue().as<Fixnum_O>()->get();
	stringstream ss;
        this->_SymbolIndex->mapHash( [this,tab,&ss] (T_sp key, T_sp value) {
                int ivalue = value.as<Fixnum_O>()->get();
                ss << string(tab,' ') << _rep_(key) << "#" << ivalue << " -> ";
                if ( ivalue == SPECIAL_TARGET )
                {
                    ss << "SPECIAL-VAR";
                } else if ( ivalue >= af_length(this->_ActivationFrame) )
                {
                    ss << "ActivationFrame->index["<<ivalue<<"]->OUT-OF-RANGE";
                } else if ( this->_ActivationFrame->boundp_entry(ivalue) )
                {
                    ss << _rep_(this->_ActivationFrame->entry(ivalue));
                } else
                {
                    ss << "UNBOUND ";
                }
                ss << std::endl;
            } );
	ss << this->Base::summaryOfContents();
	return ss.str();
    }





    /*! If the symbol is not in the lexical environment then throw an exception.
      If the symbol is lexical and was updated return true.
      If the symbol is locally special then don't update it (caller is responsible for doing that) and return false.
    */
    bool ValueEnvironment_O::updateValue(Symbol_sp sym, T_sp obj)
    {_G();
	Cons_sp it = this->_SymbolIndex->find(sym);
	if ( it.nilp() )
	{
	    Environment_sp parent = this->getParentEnvironment();
	    if ( parent.nilp() )
	    {
		SIMPLE_ERROR(BF("Could not update local symbol(%s) because it was not defined") % _rep_(sym) );
	    }
	    return nilCheck_currentVisibleEnvironment(parent)->updateValue(sym,obj);
	}
        int ivalue = oCdr(it).as<Fixnum_O>()->get();
	if ( ivalue < 0 )
	{
//	    sym->setf_symbolValue(obj);
	    return false;
	}
	this->_ActivationFrame->set_entry(ivalue,obj);
	return true;
    }


    T_sp ValueEnvironment_O::new_binding(Symbol_sp sym, int idx, T_sp obj)
    {_G();
	if ( idx < 0 )
	{
	    IMPLEMENT_MEF(BF("new_binding for special symbol[%s]") % _rep_(sym) );
	}
	if ( this->_SymbolIndex->find(sym).notnilp() )
	{
	    SIMPLE_ERROR(BF("The symbol[%s] is already in the environment") % _rep_(sym) );
	}
	this->_SymbolIndex->hash_table_setf_gethash(sym,Fixnum_O::create(idx));
	this->_ActivationFrame->set_entry(idx,obj);
	return obj;
    }




    EXPOSE_CLASS(core,ValueEnvironment_O);

    void ValueEnvironment_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<ValueEnvironment_O>()
	    .def("valueEnvironment_defineSpecialBinding",&ValueEnvironment_O::defineSpecialBinding)
	    .def("valueEnvironment_defineLexicalBinding",&ValueEnvironment_O::defineLexicalBinding)
	    ;
	af_def(CorePkg,"makeValueEnvironment",&ValueEnvironment_O::createForLambdaListHandler);
	af_def(CorePkg,"makeValueEnvironmentForNumberOfEntries",&ValueEnvironment_O::createForNumberOfEntries);
    }

    void ValueEnvironment_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,ValueEnvironment,"","",_lisp)
	    ;
#endif
    }

















    EXPOSE_CLASS(core,FunctionValueEnvironment_O);


    void FunctionValueEnvironment_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<FunctionValueEnvironment_O>()
	    .def("bindFunction",&FunctionValueEnvironment_O::bind_function)
	    ;
	af_def(CorePkg,"makeFunctionValueEnvironment",&FunctionValueEnvironment_O::createForEntries);
    }

    void FunctionValueEnvironment_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,FunctionValueEnvironment,"","",_lisp)
	    ;
#endif
    }


    ActivationFrame_sp FunctionValueEnvironment_O::getActivationFrame() const
    {
//	if (this -> isNil() ) return _Nil<ActivationFrame_O>();
	return this->_FunctionFrame;
    };




    bool FunctionValueEnvironment_O::_findFunction(T_sp functionName, int& depth, int& index, Function_sp& value) const
    {_G();
	LOG(BF("Looking for binding for function name[%s]") % _rep_(functionName) );
//    LOG(BF("The frame stack is %d deep") % this->depth() );
	bool foundp;
	Fixnum_sp val;
	{MULTIPLE_VALUES_CONTEXT();
	    T_mv mv = this->_FunctionIndices->gethash(functionName,_Nil<T_O>());
	    val = mv.as<Fixnum_O>();
	    foundp = mv.valueGet(1).isTrue();
	}
	if ( !foundp )
	{
	    return this->Base::_findFunction(functionName,depth,index,value);
	}
	index = val->get();
	LOG(BF(" Found binding %d")% index );
	value = this->_FunctionFrame->entry(index).as<Function_O>();
	return true;
    }



//
// Constructor
//

    FunctionValueEnvironment_sp FunctionValueEnvironment_O::createEmpty( Environment_sp parent )
    {_G();
        GC_ALLOCATE(FunctionValueEnvironment_O,environ );
	environ->setupParent(parent);
	return environ;
    }

    FunctionValueEnvironment_sp FunctionValueEnvironment_O::createForEntries( int numEntries, Environment_sp parent )
    {_G();
	FunctionValueEnvironment_sp environ(FunctionValueEnvironment_O::createEmpty(parent));
	environ->_FunctionFrame = FunctionFrame_O::create(numEntries,nilCheck_getActivationFrame(nilCheck_currentVisibleEnvironment(parent)));
	return environ;
    }






    void FunctionValueEnvironment_O::initialize()
    {
	this->Base::initialize();
	this->_FunctionIndices = HashTableEqual_O::create_default();
    }


#if defined(XML_ARCHIVE)
    void FunctionValueEnvironment_O::archiveBase(ArchiveP node)
    {
	IMPLEMENT_ME();
    }
#endif // defined(XML_ARCHIVE)


    class FunctionValueMapper : public KeyValueMapper
    {
    public:
	stringstream ss;
	int tab;
	FunctionValueEnvironment_O const& 	_env;

	FunctionValueMapper(int t,FunctionValueEnvironment_O const& env) : tab(t), _env(env) {};
	virtual bool mapKeyValue(T_sp key, T_sp value)
	{
	    this->ss << string(this->tab,' ') << _rep_(key) << "#" << _rep_(value);
	    int idx = value.as<Fixnum_O>()->get();
	    ss << " -> ";
	    FunctionFrame_sp fframe = this->_env.getActivationFrame().as<FunctionFrame_O>();
	    T_sp entry = fframe->entry(idx);
	    if ( entry.nilp() ) {
		ss << "NIL";
	    } else if ( entry.unboundp() ) {
		ss << "UNBOUND";
	    } else {
		Function_sp func = entry.as<Function_O>();
		ss << "function "<< _rep_(func->getFunctionName());
	    }
	    ss << std::endl;
	    return true;
	}
    };


    string FunctionValueEnvironment_O::summaryOfContents() const
    {_G();
	int tab = _sym_STARenvironmentPrintingTabSTAR->symbolValue().as<Fixnum_O>()->get();
	FunctionValueMapper mapper(tab,*this);
	this->_FunctionIndices->lowLevelMapHash(&mapper);
	stringstream ss;
	ss << mapper.ss.str();
	ss << this->Base::summaryOfContents();
	return ss.str();
    }


    int FunctionValueEnvironment_O::bind_function(T_sp functionName, Function_sp form)
    {_G();
	int nextIdx = this->_FunctionIndices->hashTableCount();
	this->_FunctionIndices->hash_table_setf_gethash(functionName,Fixnum_O::create(nextIdx));
	this->_FunctionFrame->set_entry(nextIdx,form);
	return nextIdx;
    }









    EXPOSE_CLASS(core,CompileTimeEnvironment_O);

    void CompileTimeEnvironment_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<CompileTimeEnvironment_O>()
	    ;
    }

    void CompileTimeEnvironment_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,CompileTimeEnvironment,"","",_lisp)
	    ;
#endif
    }



    CompileTimeEnvironment_O::CompileTimeEnvironment_O() : Base() {};


    ActivationFrame_sp CompileTimeEnvironment_O::getActivationFrame() const
    {_G();
	return nilCheck_getActivationFrame(this->currentVisibleEnvironment());
    };


    Environment_sp CompileTimeEnvironment_O::currentVisibleEnvironment() const
    {_G();
	Environment_sp parent = this->getParentEnvironment();
	if ( parent.nilp() ) return _Nil<Environment_O>();
	return nilCheck_currentVisibleEnvironment(parent);
    }


     bool CompileTimeEnvironment_O::_findValue(Symbol_sp sym, int& depth, int& index, bool& special, T_sp& value) const
    {_G();
     Environment_sp parent = nilCheck_currentVisibleEnvironment(this->getParentEnvironment());
     return nilCheck_findValue(parent,sym,depth,index,special,value);
    }














    UnwindProtectEnvironment_sp UnwindProtectEnvironment_O::make(Environment_sp parent)
    {_G();
	UnwindProtectEnvironment_sp environ = UnwindProtectEnvironment_O::create();
	environ->setupParent(parent);
	return environ;
    }






    EXPOSE_CLASS(core,UnwindProtectEnvironment_O);

    void UnwindProtectEnvironment_O::exposeCando(Lisp_sp lisp)
    {
	class_<UnwindProtectEnvironment_O>()
	    ;
	af_def(CorePkg,"makeUnwindProtectEnvironment",&UnwindProtectEnvironment_O::make);
    }

    void UnwindProtectEnvironment_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,UnwindProtectEnvironment,"","",_lisp)
	    ;
#endif
    }



    Environment_sp UnwindProtectEnvironment_O::find_unwindable_environment() const
    {_OF();
	return this->const_sharedThis<Environment_O>();
    }




    string UnwindProtectEnvironment_O::summaryOfContents() const
    {_G();
//	int tab = _sym_STARenvironmentPrintingTabSTAR->symbolValue().as<Fixnum_O>()->get();
	stringstream ss;
	ss << this->Base::summaryOfContents();
	return ss.str();
    }



    void UnwindProtectEnvironment_O::initialize()
    {
	this->Base::initialize();
    }


#if defined(XML_ARCHIVE)
    void UnwindProtectEnvironment_O::archiveBase(ArchiveP node)
    {
	IMPLEMENT_ME();
    }
#endif // defined(XML_ARCHIVE)




























    BlockEnvironment_sp BlockEnvironment_O::create(Environment_sp parent)
    {_G();
	BlockEnvironment_sp environ = BlockEnvironment_O::create();
	environ->setupParent(parent);
	return environ;
    }





    BlockEnvironment_sp BlockEnvironment_O::make(Symbol_sp blockSymbol, Environment_sp parent)
    {_G();
	BlockEnvironment_sp environ = BlockEnvironment_O::create(parent);
	environ->setBlockSymbol(blockSymbol);
	return environ;
    }


    EXPOSE_CLASS(core,BlockEnvironment_O);

    void BlockEnvironment_O::exposeCando(Lisp_sp lisp)
    {
	class_<BlockEnvironment_O>()
	    ;
	af_def(CorePkg,"makeBlockEnvironment",&BlockEnvironment_O::make);
    }

    void BlockEnvironment_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,BlockEnvironment,"","",_lisp)
	    ;
#endif
    }





    string BlockEnvironment_O::summaryOfContents() const
    {_G();
	int tab = _sym_STARenvironmentPrintingTabSTAR->symbolValue().as<Fixnum_O>()->get();
	stringstream ss;
	ss << string(tab,' ') << (BF("    :block-name %s\n") % _rep_(this->getBlockSymbol()) ).str();
	ss << this->Base::summaryOfContents();
	return ss.str();
    }



    void BlockEnvironment_O::initialize()
    {
	this->Base::initialize();
    }


#if defined(XML_ARCHIVE)
    void BlockEnvironment_O::archiveBase(ArchiveP node)
    {
	IMPLEMENT_ME();
    }
#endif // defined(XML_ARCHIVE)


    Environment_sp BlockEnvironment_O::find_block_named_environment(Symbol_sp blockName) const
    {_OF();
	if ( this->getBlockSymbol() == blockName ) return this->const_sharedThis<BlockEnvironment_O>();
	return this->getParentEnvironment()->find_block_named_environment(blockName);
    }


    bool BlockEnvironment_O::recognizesBlockSymbol(Symbol_sp sym) const
    {_G();
	if ( this->_BlockSymbol == sym ) return true;
	if ( this->getParentEnvironment().nilp() ) return false;
	return this->getParentEnvironment()->recognizesBlockSymbol(sym);
    }
#if 0
    int BlockEnvironment_O::getBlockSymbol(Symbol_sp sym) const
    {_G();
	if ( this->_BlockSymbol == sym ) return this->_Frame;
	if ( this->getParentEnvironment().nilp() )
	{
	    SIMPLE_ERROR(BF("Could not find block symbol %s") % _rep_(sym) );
	}
	return this->getParentEnvironment()->getBlockSymbolFrame(sym);
    }

#endif











    CatchEnvironment_sp CatchEnvironment_O::make(Environment_sp parent)
    {_G();
	CatchEnvironment_sp environ = CatchEnvironment_O::create();
	environ->setupParent(parent);
	return environ;
    }


    EXPOSE_CLASS(core,CatchEnvironment_O);

    void CatchEnvironment_O::exposeCando(Lisp_sp lisp)
    {
	class_<CatchEnvironment_O>()
	    ;
	af_def(CorePkg,"makeCatchEnvironment",&CatchEnvironment_O::make);
    }

    void CatchEnvironment_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,CatchEnvironment,"","",_lisp)
	    ;
#endif
    }





    string CatchEnvironment_O::summaryOfContents() const
    {_G();
//	int tab = _sym_STARenvironmentPrintingTabSTAR->symbolValue().as<Fixnum_O>()->get();
	stringstream ss;
	ss << this->Base::summaryOfContents();
	return ss.str();
    }



    void CatchEnvironment_O::initialize()
    {
	this->Base::initialize();
    }


#if defined(XML_ARCHIVE)
    void CatchEnvironment_O::archiveBase(ArchiveP node)
    {
	IMPLEMENT_ME();
    }
#endif // defined(XML_ARCHIVE)













    FunctionContainerEnvironment_sp FunctionContainerEnvironment_O::create(Environment_sp parent)
    {_G();
	FunctionContainerEnvironment_sp environ = FunctionContainerEnvironment_O::create();
	environ->setupParent(parent);
	return environ;
    }





    FunctionContainerEnvironment_sp FunctionContainerEnvironment_O::make(Environment_sp parent)
    {_G();
	FunctionContainerEnvironment_sp environ = FunctionContainerEnvironment_O::create(parent);
	return environ;
    }


    EXPOSE_CLASS(core,FunctionContainerEnvironment_O);

    void FunctionContainerEnvironment_O::exposeCando(Lisp_sp lisp)
    {
	class_<FunctionContainerEnvironment_O>()
	    ;
	af_def(CorePkg,"makeFunctionContainerEnvironment",&FunctionContainerEnvironment_O::make);
    }

    void FunctionContainerEnvironment_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,FunctionContainerEnvironment,"","",_lisp)
	    ;
#endif
    }





    string FunctionContainerEnvironment_O::summaryOfContents() const
    {_G();
	stringstream ss;
//	int tab = _sym_STARenvironmentPrintingTabSTAR->symbolValue().as<Fixnum_O>()->get();
	ss << this->Base::summaryOfContents();
	return ss.str();
    }



    void FunctionContainerEnvironment_O::initialize()
    {
	this->Base::initialize();
    }



    Environment_sp FunctionContainerEnvironment_O::find_current_code_environment() const
    {_OF();
	return this->const_sharedThis<FunctionContainerEnvironment_O>();
    }


    int FunctionContainerEnvironment_O::countFunctionContainerEnvironments() const
    {
	return nilCheck_countFunctionContainerEnvironments(this->getParentEnvironment())+1;
    }
















//
// Constructor
//

//
// Destructor
//

    TagbodyEnvironment_sp TagbodyEnvironment_O::make(Environment_sp parent)
    {_G();
	TagbodyEnvironment_sp environ = TagbodyEnvironment_O::create();
	environ->setupParent(parent);
     environ->_ActivationFrame = TagbodyFrame_O::create(nilCheck_getActivationFrame(nilCheck_currentVisibleEnvironment(parent)));
	return environ;
    }



    EXPOSE_CLASS(core,TagbodyEnvironment_O);

    void TagbodyEnvironment_O::exposeCando(Lisp_sp lisp)
    {
	class_<TagbodyEnvironment_O>()
	    .def("addTag",&TagbodyEnvironment_O::addTag)
	    ;
	af_def(CorePkg,"makeTagbodyEnvironment",&TagbodyEnvironment_O::make);
    }

    void TagbodyEnvironment_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,TagbodyEnvironment,"","",_lisp)
	    ;
#endif
    }


    string TagbodyEnvironment_O::summaryOfContents() const
    {_G();
	int tab = _sym_STARenvironmentPrintingTabSTAR->symbolValue().as<Fixnum_O>()->get();
	stringstream ss;
	ss << ":tagbody-id " << (void*)(this->getActivationFrame().as<TagbodyFrame_O>().get()) << std::endl;
        this->_Tags->mapHash([tab,&ss] (T_sp key, T_sp value) {
                ss << string(tab,' ') << " :tag " << _rep_(key) << std::endl;
            } );
	ss << this->Base::summaryOfContents();
	return ss.str();
    }




    void TagbodyEnvironment_O::initialize()
    {
	this->Base::initialize();
        this->_Tags = HashTableEq_O::create_default();
    }

    int TagbodyEnvironment_O::addTag(Symbol_sp tag, Cons_sp ip)
    {_OF();
	ASSERTF(this->_Tags->find(tag).nilp(),BF("The tag[%s] has already been defined in this tagbody"));
	int index = this->_TagCode.size();
	this->_Tags->hash_table_setf_gethash(tag,Fixnum_O::create(index));
	this->_TagCode.push_back(ip);
	return index;
    };

    Cons_sp TagbodyEnvironment_O::find(Symbol_sp tag) const
    {_OF();
        DEPRECIATED();
	return this->_Tags->find(tag);
    }

    string TagbodyEnvironment_O::tagsAsString() const
    {_OF();
	stringstream ss;
        this->_Tags->mapHash( [&ss] (T_sp key, T_sp value) {
                ss << _rep_(key) << " ";
            } );
	return ss.str();
    }


    ActivationFrame_sp TagbodyEnvironment_O::getActivationFrame() const
    {
	return this->_ActivationFrame;
    }





    bool TagbodyEnvironment_O::_findTag(Symbol_sp sym, int& depth, int& index) const
    {_G();
	Cons_sp it = this->_Tags->find(sym);
	if ( it.notnilp() )
	{
	    index = oCdr(it).as<Fixnum_O>()->get();
	    return true;
	}
	if ( this->getParentEnvironment().nilp() )
	{
	    return false;
	}
        ++depth;
	return nilCheck_findTag(this->getParentEnvironment(),sym,depth,index);
    }


    Cons_sp TagbodyEnvironment_O::codePos(int index) const
    {_G();
	ASSERT(index >=0 && index < this->_TagCode.size() );
	return this->_TagCode[index];
    }



    Environment_sp TagbodyEnvironment_O::find_tagbody_tag_environment(Symbol_sp tag) const
    {_OF();
	Cons_sp it = this->_Tags->find(tag);
	if ( it.notnilp() )
	{
	    return this->const_sharedThis<TagbodyEnvironment_O>();
	}
	return this->getParentEnvironment()->find_tagbody_tag_environment(tag);
    }




    GlueEnvironment_sp GlueEnvironment_O::create(Cons_sp parts)
    {_G();
	GlueEnvironment_sp env(GlueEnvironment_O::create());
	ql::list args(_lisp);
	for ( Cons_sp cur=parts; cur.notnilp(); cur=cCdr(cCdr(cur)))
	{
	    Symbol_sp sym = oCar(cur).as<Symbol_O>();
	    T_sp val = oCadr(cur);
	    env->_Map->hash_table_setf_gethash(sym,val);
	    args << val;
	}
	env->_Args = args.cons();
	return env;
    }







//
// Constructor
//

//
// Destructor
//

    MacroletEnvironment_sp MacroletEnvironment_O::make(Environment_sp parent)
    {_G();
	MacroletEnvironment_sp environ = MacroletEnvironment_O::create();
	environ->setupParent(parent);
	return environ;
    }



    EXPOSE_CLASS(core,MacroletEnvironment_O);

    void MacroletEnvironment_O::exposeCando(Lisp_sp lisp)
    {
	class_<MacroletEnvironment_O>()
	    .def("addMacro",&MacroletEnvironment_O::addMacro)
	    ;
	af_def(CorePkg,"makeMacroletEnvironment",&MacroletEnvironment_O::make);
    }

    void MacroletEnvironment_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,MacroletEnvironment,"","",_lisp)
	    ;
#endif
    }


    void MacroletEnvironment_O::initialize()
    {_G();
	this->Base::initialize();
        this->_Macros = HashTableEq_O::create_default();
    }

    string MacroletEnvironment_O::summaryOfContents() const
    {_G();
	int tab = _sym_STARenvironmentPrintingTabSTAR->symbolValue().as<Fixnum_O>()->get();
	stringstream ss;
        this->_Macros->mapHash( [tab,&ss] (T_sp key, T_sp value) {
                ss << string(tab,' ') << _rep_(key) << std::endl;
            } );
	ss << this->Base::summaryOfContents();
	return ss.str();
    }





    bool MacroletEnvironment_O::_findMacro(Symbol_sp sym, int& depth, int& index, Function_sp& value) const
    {_G();
	LOG(BF("Looking for binding for symbol(%s)") % _rep_(sym) );
//    LOG(BF("The frame stack is %d deep") % this->depth() );
	Cons_sp fi = this->_Macros->find(sym);
	if ( fi.nilp() )
	{
	    return this->Base::_findMacro(sym,depth,index,value);
	}
	LOG(BF(" Found binding %s")% fi->second );
	value = oCdr(fi).as<Function_O>();
	return true;
    }



    void MacroletEnvironment_O::addMacro(Symbol_sp sym, Function_sp macro)
    {_G();
	this->_Macros->hash_table_setf_gethash(sym,macro);
    }




    SymbolMacroletEnvironment_sp SymbolMacroletEnvironment_O::make(Environment_sp parent)
    {_G();
	SymbolMacroletEnvironment_sp environ = SymbolMacroletEnvironment_O::create();
	environ->setupParent(parent);
	return environ;
    }


    bool SymbolMacroletEnvironment_O::_findSymbolMacro(Symbol_sp sym, int& depth, int& index, bool& shadowed, Function_sp& value) const
    {_G();
	LOG(BF("Looking for binding for symbol(%s)") % _rep_(sym) );
//    LOG(BF("The frame stack is %d deep") % this->depth() );
	Cons_sp fi = this->_Macros->find(sym);
	if ( fi.nilp() )
	{
	    return this->Base::_findSymbolMacro(sym,depth,index,shadowed,value);
	}
	LOG(BF(" Found binding %s")% fi->second );
	value = oCdr(fi).as<Function_O>();
	shadowed = false;
	return true;
    }


    void SymbolMacroletEnvironment_O::addSymbolMacro(Symbol_sp sym, Function_sp expansion)
    {_G();
	this->_Macros->hash_table_setf_gethash(sym,expansion);
    }


    EXPOSE_CLASS(core,SymbolMacroletEnvironment_O);

    void SymbolMacroletEnvironment_O::exposeCando(Lisp_sp lisp)
    {
	class_<SymbolMacroletEnvironment_O>()
	    .def("addSymbolMacro",&SymbolMacroletEnvironment_O::addSymbolMacro)
	    ;
	af_def(CorePkg,"makeSymbolMacroletEnvironment",&SymbolMacroletEnvironment_O::make);
    }

    void SymbolMacroletEnvironment_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,SymbolMacroletEnvironment,"","",_lisp)
	    ;
#endif
    }


    void SymbolMacroletEnvironment_O::initialize()
    {_G();
	this->Base::initialize();
        this->_Macros = HashTableEq_O::create_default();
    }


    string SymbolMacroletEnvironment_O::summaryOfContents() const
    {_G();
	int tab = _sym_STARenvironmentPrintingTabSTAR->symbolValue().as<Fixnum_O>()->get();
	stringstream ss;
	this->_Macros->mapHash( [tab,&ss] (T_sp key, T_sp value ) {
                ss << string(tab,' ') << _rep_(key);
                ss << " --> " << _rep_(value);
                ss << std::endl;
            } );
	ss << this->Base::summaryOfContents();
	return ss.str();
    }














    REGISTER_CLASS(core,GlueEnvironment_O);
 
    void GlueEnvironment_O::exposeCando(Lisp_sp lisp)
    {
	class_<GlueEnvironment_O>()
	    ;
    }


    void GlueEnvironment_O::initialize()
    {
        this->Base::initialize();
        this->_Map = HashTableEq_O::create_default();
    }


    T_mv GlueEnvironment_O::variable_lookup(Symbol_sp val) const
    {_G();
	Cons_sp it = this->_Map->find(val);
	return(Values(oCdr(it),_lisp->_true()));
    }










}; // namespace core



