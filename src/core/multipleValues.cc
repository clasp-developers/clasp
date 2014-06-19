#define	DEBUG_LEVEL_FULL

#include "core/foundation.h"
#include "core/common.h"
#include "core/environment.h"
#include "evaluator.h"
#include "multipleValues.h"
#include "vectorObjects.h"
#include "core/wrappers.h"
namespace core
{

    const int MultipleValues::MultipleValuesLimit;


    void MultipleValues::initialize()
    {
        this->_Values.reserve(MultipleValuesLimit);
    };


    T_sp MultipleValues::valueGet(int idx,int number_of_values) const
    {
	ASSERTF(idx>=0,BF("multiple-value.valueGet index[%d] must be larger than 0") % idx );
	if ( idx < number_of_values ) return this->_Values[idx];
//	printf("%s:%d - WARNING: You asked for multiple-value[%d] and there are only %d values - turn this off once everything is working\n", __FILE__, __LINE__, idx, number_of_values);
	return _Nil<T_O>();
    }


  

    T_sp MultipleValues::setFromConsSkipFirst(Cons_sp args)
    {_G();
	// Skip the first value - that is in multiple_values<XXX>
	int i=1;
        SUPPRESS_GC();
        this->setSize(MultipleValuesLimit);
	for ( Cons_sp cur = cCdr(args); cur.notnilp(); cur=cCdr(cur) )
	{
	    if ( i>= MultipleValues::MultipleValuesLimit )
	    {
		SIMPLE_ERROR(BF("Overflow when returning multiple values - only %d are supported and you tried to return %d values") % MultipleValuesLimit % af_length(args) );
	    }
	    T_sp obj = oCar(cur);
	    this->valueSet(i,obj);
	    ++i;
	}
        this->setSize(i);
        ENABLE_GC();
	return oCar(args);
    }



    Cons_sp MultipleValues::asCons(int iend) const
    {_OF();
	Cons_sp cur = _Nil<Cons_O>();
	for ( int i=iend-1; i>0; --i )
	{
	    Cons_sp one = _lisp->create<Cons_O>(this->_Values[i]);
	    cur->setCdr(one);
	    cur = one;
	}
	return cur;
    }

#if 0
    GC_RESULT MultipleValues::scanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
    {
	GC_SCANNER_BEGIN() {
	    for ( size_t i=0; i<this->_Size; ++i )
	    {
		SMART_PTR_FIX(this->_Values[i]);
	    }
	} GC_SCANNER_END();
	return GC_RES_OK;
    }
#endif



    void multipleValuesSaveToVector(T_mv values, VectorObjects_sp save)
    {
	core::MultipleValues* mv = core::lisp_multipleValues();
	save->adjust(_Nil<T_O>(),_Nil<Cons_O>(),values.number_of_values());
	if ( values.number_of_values() > 0 )
	{
	    save->operator[](0) = values;
	}
	for ( int i(1); i<values.number_of_values(); ++i)
	{
	    save->operator[](i) = mv->valueGet(i,values.number_of_values());
	}
    }


    T_mv multipleValuesLoadFromVector(VectorObjects_sp load)
    {
	if ( af_length(load) > 0 )
	{
	    T_mv mvn(load->operator[](0),af_length(load));
	    core::MultipleValues* mv = lisp_multipleValues();
            SUPPRESS_GC();
            int i(0);
            int iEnd(af_length(load));
            mv->setSize(iEnd);
	    for (; i<iEnd; ++i ) {mv->valueSet(i,load->operator[](i));}
            ENABLE_GC();
	    return mvn;
	}
	T_mv mvNil(_Nil<T_O>(),0);
	return mvNil;
    }







}; /* core */



    core::T_mv ValuesFromCons(core::Cons_sp vals)
    {
	size_t len = af_length(vals);
	if ( len == 0 )
	{
	    return core::T_mv(_Nil<core::T_O>(),0);
	}
	core::MultipleValues& me = *(core::lisp_multipleValues());
	core::T_sp first = me.setFromConsSkipFirst(vals);
	core::T_mv mv;
	mv = gctools::multiple_values<core::T_O>(first,len);
	return mv;
    }


