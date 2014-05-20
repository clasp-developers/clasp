
#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "setfExpander.h"

namespace core
{

namespace macros
{

    T_sp setf(Symbol_sp accessor, T_sp target, T_sp val, Lisp_sp lisp)
    {_G();
	SetfExpander_sp expander = _lisp->lookupSetfExpander(accessor);
	return expander->invoke(target,val);
    }


    T_sp push(Symbol_sp accessor, T_sp target, T_sp val, Lisp_sp lisp)
    {_G();
	SetfExpander_sp expander = _lisp->lookupSetfExpander(accessor);
	Cons_sp tlist = target->slot_value(accessor).as_or_nil<Cons_O>();
	tlist = Cons_O::create(val,tlist);
	return expander->invoke(target,tlist);
    }


    T_sp pushnew(Symbol_sp accessor, T_sp target, T_sp val, T_sp test_funcDesig, T_sp key_funcDesig, Lisp_sp lisp)
    {_G();
	IMPLEMENT_MEF(BF("implement macros::pushnew"));
#if 0
	SetfExpander_sp expander = _lisp->lookupSetfExpander(accessor);
	Cons_sp tlist = target->slot_value(accessor);
	for ( Cons_sp cur =tlist; cur.notnilp(); cur=cCdr(cur) )
	{
	    WORKING WORKING WORKING 
	tlist = Cons_O::create(val,tlist);
	return expander->invoke(target,tlist);
#endif
    }


p
}; // namespace macros
}; // namespace core
