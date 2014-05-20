#ifndef	methodptr_H
#define	methodptr_H

#include "foundation.h"
#include "object.h"


namespace core
{


template <typename OT>
class OldStyleMethoid : public SingleDispatchMethoid
{
private:
	T_sp (OT::*fptr)(Function_sp,Cons_sp, Environment_sp, Lisp_sp );
public:
    virtual string describe() const {return "OldStyleMethoid";};
    OldStyleMethoid( const string& name, T_sp (OT::*fp)(Function_sp,Cons_sp, Environment_sp, Lisp_sp ) ) : SingleDispatchMethoid("OldStyleMethoid->"+name)
    {
	this->fptr = fp;
    };
};




/*! Methods that don't need to have argument/result translation are
 * stored as Methoid objects
 */
#if 0
template <typename OT>
class Methoid : public Functoid
{
private:
	T_sp (OT::*fptr)(Function_sp,Cons_sp, Environment_sp, Lisp_sp );
public:
    Methoid( T_sp (OT::*fp)(Function_sp,Cons_sp, Environment_sp, Lisp_sp ) )
    {
	this->fptr = fp;
    };
    virtual string describe() const {return "Methoid";};
};
#else
template <typename OT>
class Methoid : public SingleDispatchMethoid
{
private:
    T_mv (OT::*fptr)(ActivationFrame_sp frame,int singleDispatchArgumentIndex);
public:
    Methoid(T_mv (OT::*fp)(ActivationFrame_sp) )
    {
	this->fptr = fp;
    };
    virtual string describe() const {return "Methoid";};
    T_mv activate( ActivationFrame_sp closedEnv,const_ActivationFrame_spREF frame)
    {
	T_sp receiver = frame->entry(this->_SingleDispatchArgumentIndex);
	mem::smart_ptr<OT> obj = receiver.as<OT>();
	T_mv result = ((obj.get())->*fptr)(frame,this->_SingleDispatchArgumentIndex);
	return result;
    }
};
#endif


/*! A ChainableMethoid is a Methoid that is called with two arguments: (args next-emfun)
  It's methoid is applied to args and next-emfun is ignored
 */
#if 0
class ChainableMethoid : public Functoid
{
private:
	Functoid* _methoid; // take ownership
public:
    ChainableMethoid( Functoid* m) : _methoid(m) {};
    virtual ~ChainableMethoid() { if (this->_methoid!=NULL) { delete this->_methoid; this->_methoid = NULL;}};
    virtual string describe() const {return "ChainableMethoid";};
};
#else
class ChainableMethoid : public AFFunctoid
{
private:
	Functoid* _methoid; // take ownership
public:
    ChainableMethoid( const string& name, Functoid* m) : AFFunctoid(name), _methoid(m) {};
    virtual ~ChainableMethoid() { if (this->_methoid!=NULL) { delete this->_methoid; this->_methoid = NULL;}};
    virtual string describe() const {return "ChainableMethoid";};
    T_mv activate( ActivationFrame_sp closedEnv,int nargs, ArgArray args)
    {_G();
	IMPLEMENT_MEF(BF("Handle new way of passing emfun in activation frames"));
#if 0
	Cons_sp methodArgs = args->ocar().as_or_nil<Cons_O>();
	Function_sp next_emfun = args->ocadr().as<Function_O>();
	// I can ignore next_emfun because once we enter C++ code calling parent/next functions
	// is handled by the virtual function machinery
	ASSERTF(this->_methoid!=NULL,BF("The methoid of a ChainableMethoid is UNDEFINED - this should never happen!!!"));
	return this->_methoid->invoke(e,methodArgs,env,lisp);
#endif
    }
};
#endif



};
#endif // methodptr_H
