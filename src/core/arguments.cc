/*
    File: arguments.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
#include "foundation.h"
#include "object.h"
#include "symbolTable.h"
#include "lambdaListHandler.h"
#include "activationFrame.h"
#include "arguments.h"

namespace core
{

    Cons_sp Argument::lambdaList() const
    {
	return((this->_ArgTarget.as_or_nil<Cons_O>()));
    };

    Symbol_sp Argument::symbol() const
    {
	return((this->_ArgTarget.as<Symbol_O>()));
    };

    Cons_sp Argument::classified() const
    {_G();
	if ( this->_ArgTargetFrameIndex == SPECIAL_TARGET )
	{
	    return((Cons_O::create(ext::_sym_specialVar,this->_ArgTarget)));
	} else if ( this->_ArgTargetFrameIndex >= 0 )
	{
	    return((Cons_O::create(ext::_sym_heapVar,Cons_O::create(this->_ArgTarget,Fixnum_O::create(this->_ArgTargetFrameIndex)))));
	} else if ( this->_ArgTargetFrameIndex == UNDEFINED_TARGET )
	{
	    return((_Nil<Cons_O>()));
	}
	SIMPLE_ERROR(BF("Illegal target"));
    }

    LambdaListHandler_sp Argument::lambdaListHandler() const
    {
	return((this->_ArgTarget.as<LambdaListHandler_O>()));
    }


    string Argument::asString() const
    {
	stringstream ss;
	ss << "#<Argument ";
	ss << ":target ";
	ss << _rep_(this->_ArgTarget);
	ss << " :tfi ";
	ss << this->_ArgTargetFrameIndex;
	ss << " >  ";
	return((ss.str()));
    }

    string ArgumentWithDefault::asString() const
    {
	stringstream ss;
	ss << "#<ArgumentWithDefault ";
	ss << ":target ";
	ss << _rep_(this->_ArgTarget);
	ss << " :tfi ";
	ss << this->_ArgTargetFrameIndex;
	ss << " :default ";
	ss << _rep_(this->_Default);
	ss << " >  ";
	return((ss.str()));
    }




    string RequiredArgument::asString() const
    {
	stringstream ss;
	ss << "#<RequiredArgument ";
	ss << ":target ";
	this->Base::asString();
	ss << " >  ";
	return((ss.str()));
    }


    string OptionalArgument::asString() const
    {
	stringstream ss;
	ss << "#<OptionalArgument ";
	ss << ":target ";
	ss << this->Base::asString();
	if ( this->_Sensor.isDefined() )
	{
	    ss << " :sensor ";
	    ss << this->_Sensor.asString();
	}
	ss << " >  ";
	return((ss.str()));
    }


    string RestArgument::asString() const
    {
	stringstream ss;
	ss << "#<RestArgument ";
	ss << ":target ";
	ss << _rep_(this->_ArgTarget);
	ss << " :tfi ";
	ss << this->_ArgTargetFrameIndex;
	ss << " >  ";
	return((ss.str()));
    }


    string KeywordArgument::asString() const
    {
	stringstream ss;
	ss << "#<KeywordArgument ";
	ss << ":keyword " << _rep_(this->_Keyword);
	ss << " :target ";
	ss << this->Base::asString();
	if ( this->_Sensor.isDefined() )
	{
	    ss << " :sensor ";
	    ss << this->_Sensor.asString();
	}
	ss << " >  ";
	return((ss.str()));
    }



    string AuxArgument::asString() const
    {
	stringstream ss;
	ss << "#<AuxArgument ";
	ss << ":target ";
	this->Base::asString();
	ss << " :expression ";
	ss << _rep_(this->_Expression);
	ss << " >  ";
	return((ss.str()));
    }






    DynamicScopeManager::DynamicScopeManager()
    {
	int top = _lisp->bindings().top();
	this->_beginTop = top;
	this->_endTop = top;
    }




    DynamicScopeManager::DynamicScopeManager(Symbol_sp sym, T_sp val)
    {
	int top = _lisp->bindings().top();
	this->_beginTop = top;
	this->_endTop = top;
	this->pushSpecialVariableAndSet(sym,val);
    }

    void DynamicScopeManager::dump() const
    {
	stringstream ss;
	ss << "DynamicScopeManager  _beginTop[" << this->_beginTop << "] _endTop[" << this->_endTop << "]" << std::endl;
	for ( int i=this->_endTop-1; i>=this->_beginTop; --i )
	{
	    ss << (BF("Dynamic[%d] var[%s] val-->%s") % i % _rep_(_lisp->bindings().var(i)) % _rep_(_lisp->bindings().val(i)) ).str() << std::endl;
	}
	printf("%s",ss.str().c_str());
    }

    void DynamicScopeManager::pushSpecialVariableAndSet(Symbol_sp sym, T_sp val)
    {
	_lisp->bindings().push(sym);
	this->_endTop = _lisp->bindings().top();
//	SymbolSaveValue sv(sym,sym->symbolValueUnsafe());
//	this->_SavedValues.push_back(sv);
	sym->setf_symbolValue(val);
    }


    /*! The frame_index is not used here - it is only used by ActivationFrameDynamicLexicalScopeManager */
    void DynamicScopeManager::new_binding(const Argument& arg, T_sp val)
    {
	if ( arg._ArgTargetFrameIndex == SPECIAL_TARGET )
	{
	    Symbol_sp sym = arg._ArgTarget.as<Symbol_O>();
	    this->pushSpecialVariableAndSet(sym,val);
	    return;
	}
	SIMPLE_ERROR(BF("DynamicScopeManager doesn't bind anything other than SPECIAL_TARGET bindings - you gave it a binding to[%s] index[%d]") % _rep_(arg._ArgTarget) % arg._ArgTargetFrameIndex);
    }

    T_sp DynamicScopeManager::lexenv() const
    {
	SIMPLE_ERROR(BF("A ValueEnvironment was requested from a DynamicScopeManager - only ValueEnvironmentDynamicScopeManagers have those"));
    }

    DynamicScopeManager::~DynamicScopeManager()
    {
	DynamicBindingStack& bindings = _lisp->bindings();
	int numBindings = this->_endTop-this->_beginTop;
	for ( int i=0; i<numBindings; ++i)
	{
	    bindings.pop();
	}
#if 0	
	for ( vector<SymbolSaveValue>::iterator it=this->_SavedValues.begin();
	      it!=this->_SavedValues.end(); it++ )
	{
	    it->_Symbol->setf_symbolValue(it->_SavedValue);
	}
#endif
    }




    bool ValueEnvironmentDynamicScopeManager::lexicalElementBoundP(const Argument& argument)
    {
	return((this->_Environment->activationFrameElementBoundP(argument._ArgTargetFrameIndex)));
    }


    void ValueEnvironmentDynamicScopeManager::new_binding(const Argument& argument, T_sp val)
    {
	if ( argument._ArgTargetFrameIndex == SPECIAL_TARGET )
	{
	    this->DynamicScopeManager::new_binding(argument,val);
	    return;
	}
	ASSERTF(argument._ArgTargetFrameIndex >= 0, BF("Illegal ArgTargetIndex[%d] for lexical variable[%s]") % argument._ArgTargetFrameIndex % _rep_(argument._ArgTarget) );
	this->_Environment->new_binding(argument._ArgTarget.as<Symbol_O>(),argument._ArgTargetFrameIndex,val);
    }



    void ValueEnvironmentDynamicScopeManager::new_variable(Cons_sp classified, T_sp val)
    {
	Symbol_sp type = oCar(classified).as<Symbol_O>();
	if ( type == ext::_sym_specialVar )
	{
	    Symbol_sp sym = oCdr(classified).as<Symbol_O>();
	    this->DynamicScopeManager::pushSpecialVariableAndSet(sym,val);
	    return;
	} else if ( type == ext::_sym_heapVar )
	{
	    Symbol_sp sym = oCadr(classified).as<Symbol_O>();
	    int idx = oCddr(classified).as<Fixnum_O>()->get();
	    ASSERTF(idx >= 0, BF("Illegal target index[%d] for lexical variable[%s]") % idx % sym );
	    this->_Environment->new_binding(sym,idx,val);
	    return;
	}
	SIMPLE_ERROR(BF("Illegal classified type: %s\n") % _rep_(classified) );
    }


    void ValueEnvironmentDynamicScopeManager::new_special(Cons_sp classified)
    {
	ASSERT(oCar(classified)==_sym_declaredSpecial);
	Symbol_sp sym = oCdr(classified).as<Symbol_O>();
	this->_Environment->defineSpecialBinding(sym);
    }

    










    void ActivationFrameDynamicScopeManager::new_binding(const Argument& argument, T_sp val)
    {
	if ( argument._ArgTargetFrameIndex == SPECIAL_TARGET )
	{
	    this->DynamicScopeManager::new_binding(argument,val);
	    return;
	}
	ASSERTF(argument._ArgTargetFrameIndex >= 0, BF("Illegal ArgTargetIndex[%d] for lexical variable[%s]") % argument._ArgTargetFrameIndex % _rep_(argument._ArgTarget) );
	this->_Frame->set_entry(argument._ArgTargetFrameIndex,val);
    }


    bool ActivationFrameDynamicScopeManager::lexicalElementBoundP(const Argument& argument)
    {
	return((this->_Frame->boundp_entry(argument._ArgTargetFrameIndex)));
    }


    T_sp ActivationFrameDynamicScopeManager::lexenv() const
    {
//	SIMPLE_ERROR(BF("A ValueEnvironment was requested from a DynamicScopeManager... \n but only ValueEnvironmentDynamicScopeManagers have those.   \n On the other hand, ActivationFrameDynamicScopeManager::lexenv() \n should only be called when evaluating lambda-list init-forms \n (&optional,&key,&aux) and those should be evaluated in an environment \n that only has the lambda-list bindings in the top-level-environment \n - Since we attach the binding symbol names to the ActivationFrame we \n could just use the ActivationFrame of this ActivationFrameDynamicScopeManager \n as the environment that lexenv returns"));
	// I'm going to return the ActivationFrame here and ASSERT that it must have debugging info
	// attached.  I don't think the caller should be evaluating expressions in the environment
	// represented by this->_Frame unless it has symbol names attached to it.
	// I'm not sure the ActivationFrames with debugging information honor all of the
	// variable/function lookup and update functions though so even with debugging information
	// providing symbol names of variables it may not work - meister Nov 2013
	return this->_Frame;
    }




    void StackFrameDynamicScopeManager::new_binding(const Argument& argument, T_sp val)
    {
	if ( argument._ArgTargetFrameIndex == SPECIAL_TARGET )
	{
	    this->DynamicScopeManager::new_binding(argument,val);
	    return;
	}
	ASSERTF(argument._ArgTargetFrameIndex >= 0, BF("Illegal ArgTargetIndex[%d] for lexical variable[%s]") % argument._ArgTargetFrameIndex % _rep_(argument._ArgTarget) );
        core::T_O** array(frame::ValuesArray(this->frame));
        array[argument._ArgTargetFrameIndex] = val.asTPtr();
    }


    bool StackFrameDynamicScopeManager::lexicalElementBoundP(const Argument& argument)
    {
        core::T_O** array(frame::ValuesArray(this->frame));
	return !gctools::tagged_ptr<T_O>::tagged_unboundp(array[argument._ArgTargetFrameIndex]);
    }


    T_sp StackFrameDynamicScopeManager::lexenv() const
    {
        return this->frame;
    }


    T_sp StackFrameDynamicScopeManager::activationFrame() const
    {
        return this->frame;
    }

	
};
