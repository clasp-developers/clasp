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


#if 0
/*! A ChainableMethoid is a Methoid that is called with two arguments: (args next-emfun)
  It's methoid is applied to args and next-emfun is ignored
 */
class ChainableMethoid : public Closure
{
private:
	Functoid* _methoid; // take ownership
public:
    ChainableMethoid( const string& name, Functoid* m) : AFFunctoid(name), _methoid(m) {};
    DISABLE_NEW();
    virtual ~ChainableMethoid() { if (this->_methoid!=NULL) { delete this->_methoid; this->_methoid = NULL;}};
    virtual string describe() const {return "ChainableMethoid";};
    T_mv activate( ActivationFrame_sp closedEnv,int nargs, ArgArray args)
    {_G();
	IMPLEMENT_MEF(BF("Handle new way of passing emfun in activation frames"));
    }
};
#endif


};
#endif // methodptr_H
