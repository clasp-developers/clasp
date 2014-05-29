#ifndef	_core_MultipleValues_H
#define _core_MultipleValues_H


namespace core
{


#pragma GCC visibility push(default)
    class MultipleValues : public gctools::GC_Manual
    {
    public: // ctor
	MultipleValues();
	static const int MultipleValuesLimit = 32;
    public: // instance variables here
        int     _Size;
	T_sp	_Values[MultipleValuesLimit]; 		// TODO: Make sure this is a root
    public: // Functions here

	/*! Return the indexed multiple value or nil */
	ATTR_WEAK T_sp valueGet(int idx,int number_of_arguments) const;

        GC_RESULT scanGCRoots(GC_SCAN_ARGS_PROTOTYPE);

	T_sp setFromConsSkipFirst(Cons_sp values);

        void setMaxSize() { this->_Size = MultipleValuesLimit;};
        void setSize(int sz) { this->_Size = sz;};
        int getSize() const { return this->_Size;};
	/*! Set the value */
	void valueSet(int i, const T_sp& val)
        {
            this->_Values[i] = val;
        }

	/*! Return a Cons of elements 1 up to but not including iend */
	Cons_sp asCons(int iend) const;

        void saveToVec0(gctools::Vec0<core::T_sp>& vec)
        {
            vec.resize(this->_Size);
            for ( int i(0), iEnd(this->_Size); i<iEnd; ++i ) {
                vec[i] = this->_Values[i];
            }
        }
        void loadFromVec0(const gctools::Vec0<core::T_sp>& vec)
        {
            this->setSize(vec.size());
            for ( int i(0), iEnd(vec.size()); i<iEnd; ++i ) {
                this->_Values[i] = vec[i];
            };
        };

    };
#pragma GCC visibility pop



};



namespace mem
{



    template <class T>
    class multiple_values : public smart_ptr<T>
    {
    private:
	int 	_number_of_values;
    public:
	multiple_values() : smart_ptr<T>(_Nil<T>()), _number_of_values(0) {};
	multiple_values(const smart_ptr<T>& v,int num) : smart_ptr<T>(v), _number_of_values(num) {};
	multiple_values(const smart_ptr<T>& v) : smart_ptr<T>(v), _number_of_values(1) {};
	template <class Y> multiple_values(const mem::multiple_values<Y>& yy) : smart_ptr<T>(yy), _number_of_values(yy.number_of_values()) {};



        static multiple_values<T> createFromValues() {
	    core::MultipleValues* mv = core::lisp_multipleValues();
            multiple_values<T> result( mv->getSize()==0 ? _Nil<core::T_O>() : mv->valueGet(0,mv->getSize()), mv->getSize());
            return result;
        }


        static multiple_values<T> createFromVec0(const gctools::Vec0<core::T_sp>& vec) {
	    core::MultipleValues* mv = core::lisp_multipleValues();
            mv->loadFromVec0(vec);
            return mem::multiple_values<T>::createFromValues();
        }


        void saveToMultipleValue0() const {
	    core::MultipleValues* mv = core::lisp_multipleValues();
            mv->valueSet(0,*this);
        };

        void saveToVec0(gctools::Vec0<core::T_sp>& values) {
            values.resize(this->_number_of_values);
            values[0] = *this;
            for ( int i(1); i<this->_number_of_values; ++i ) {
                values[i] = this->valueGet(i);
            }
        }


#ifdef POLYMORPHIC_SMART_PTR
	virtual
#endif
	int number_of_values() const { return this->_number_of_values;};

	void valueSet(int idx, smart_ptr<core::T_O> val)
	{
	    core::MultipleValues* mv = core::lisp_multipleValues();
	    mv->valueSet(idx,val);
	}
	    
	core::T_sp valueGet(int idx) const
	{
	    core::MultipleValues* mv = core::lisp_multipleValues();
	    core::T_sp val = mv->valueGet(idx,this->_number_of_values);
	    return val;
	};


	void dump() 
	{
	    if (this->_number_of_values > 0 )
	    {
		string ts = (*this)->__repr__();
		printf(" %s\n", ts.c_str() );
		for (int i(1); i<this->_number_of_values; ++i )
		{
		    string ts = _rep_(this->valueGet(i));
		    printf(" %s\n", ts.c_str() );
		}
	    } else
	    {
		printf( "---No values---\n");
	    }
	}


            



    };

#if defined(USE_MPS)
    template <class TO, class FROM>
    multiple_values<TO> dynamic_pointer_cast(const multiple_values<FROM>& ptr)
    {
	smart_ptr<FROM> sp = ptr;
	return multiple_values<TO>(mem::dynamic_pointer_cast<TO>(sp),ptr.number_of_values());
    };
#else
    template <class TO, class FROM>
    multiple_values<TO> dynamic_pointer_cast(const multiple_values<FROM>& ptr)
    {
	smart_ptr<FROM> sp = ptr;
	return multiple_values<TO>(boost::dynamic_pointer_cast<TO>(sp),ptr.number_of_values());
    };
#endif


};




namespace core
{
    typedef mem::multiple_values<T_O>	T_mv;
};



namespace core
{
    void multipleValuesSaveToVector(core::T_mv values, core::VectorObjects_sp save);


    core::T_mv multipleValuesLoadFromVector(core::VectorObjects_sp load);


};


template <typename T0>
static mem::multiple_values<T0> ValuesFromArray(core::T_sp vals[], int sz)
{
    core::MultipleValues& me= *(core::lisp_multipleValues());
    SUPPRESS_GC();
    me.setMaxSize();
    for ( int i(1); i<sz; i++ ) me.valueSet(i,vals[i]);
    me.setSize(sz);
    ENABLE_GC();
    return mem::multiple_values<T0>(vals[0].as<T0>(),sz);
}


extern core::T_mv ValuesFromCons(core::Cons_sp vals);

    template <class T0,class T1,class T2,class T3, class T4, class T5,class T6,class T7,class T8,class T9>
    static mem::multiple_values<T0> Values(const mem::smart_ptr<T0>& v0,
					   const mem::smart_ptr<T1>& v1,
					   const mem::smart_ptr<T2>& v2,
					   const mem::smart_ptr<T3>& v3,
					   const mem::smart_ptr<T4>& v4,
					   const mem::smart_ptr<T5>& v5,
					   const mem::smart_ptr<T6>& v6,
					   const mem::smart_ptr<T7>& v7,
					   const mem::smart_ptr<T8>& v8,
					   const mem::smart_ptr<T9>& v9)
    {
	core::MultipleValues& me = *(core::lisp_multipleValues());
        SUPPRESS_GC();
        me.setMaxSize();
	me.valueSet(1, v1);
	me.valueSet(2, v2);
	me.valueSet(3, v3);
	me.valueSet(4, v4);
	me.valueSet(5, v5);
	me.valueSet(6, v6);
	me.valueSet(7, v7);
	me.valueSet(8, v8);
	me.valueSet(9, v9);
        me.setSize(10);
        ENABLE_GC();
	return mem::multiple_values<T0>(v0,10);
    }


    template <class T0,class T1,class T2,class T3, class T4, class T5,class T6,class T7,class T8>
    static mem::multiple_values<T0> Values(const mem::smart_ptr<T0>& v0,
					   const mem::smart_ptr<T1>& v1,
					   const mem::smart_ptr<T2>& v2,
					   const mem::smart_ptr<T3>& v3,
					   const mem::smart_ptr<T4>& v4,
					   const mem::smart_ptr<T5>& v5,
					   const mem::smart_ptr<T6>& v6,
					   const mem::smart_ptr<T7>& v7,
					   const mem::smart_ptr<T8>& v8
	)
    {
	core::MultipleValues& me = *(core::lisp_multipleValues());
	SUPPRESS_GC();
        me.valueSet(1, v1);
	me.valueSet(2, v2);
	me.valueSet(3, v3);
	me.valueSet(4, v4);
	me.valueSet(5, v5);
	me.valueSet(6, v6);
	me.valueSet(7, v7);
	me.valueSet(8, v8);
        ENABLE_GC();
	return mem::multiple_values<T0>(v0,9);
    }


    template <class T0,class T1,class T2,class T3, class T4, class T5,class T6,class T7>
    static mem::multiple_values<T0> Values(const mem::smart_ptr<T0>& v0,
					   const mem::smart_ptr<T1>& v1,
					   const mem::smart_ptr<T2>& v2,
					   const mem::smart_ptr<T3>& v3,
					   const mem::smart_ptr<T4>& v4,
					   const mem::smart_ptr<T5>& v5,
					   const mem::smart_ptr<T6>& v6,
					   const mem::smart_ptr<T7>& v7
	)
    {
	core::MultipleValues& me = *(core::lisp_multipleValues());
	SUPPRESS_GC();
        me.setMaxSize();
        me.valueSet(1, v1);
	me.valueSet(2, v2);
	me.valueSet(3, v3);
	me.valueSet(4, v4);
	me.valueSet(5, v5);
	me.valueSet(6, v6);
	me.valueSet(7, v7);
	me.setSize(8);
        ENABLE_GC();
        return mem::multiple_values<T0>(v0,8);
    }


    template <class T0,class T1,class T2,class T3, class T4, class T5,class T6>
    static mem::multiple_values<T0> Values(const mem::smart_ptr<T0>& v0,
					   const mem::smart_ptr<T1>& v1,
					   const mem::smart_ptr<T2>& v2,
					   const mem::smart_ptr<T3>& v3,
					   const mem::smart_ptr<T4>& v4,
					   const mem::smart_ptr<T5>& v5,
					   const mem::smart_ptr<T6>& v6
	)
    {
	core::MultipleValues& me = *(core::lisp_multipleValues());
	SUPPRESS_GC();
        me.setMaxSize();
        me.valueSet(1, v1);
	me.valueSet(2, v2);
	me.valueSet(3, v3);
	me.valueSet(4, v4);
	me.valueSet(5, v5);
	me.valueSet(6, v6);
	me.setSize(7);
        ENABLE_GC();
        return mem::multiple_values<T0>(v0,7);
    }


    template <class T0,class T1,class T2,class T3, class T4, class T5>
    static mem::multiple_values<T0> Values(const mem::smart_ptr<T0>& v0,
					   const mem::smart_ptr<T1>& v1,
					   const mem::smart_ptr<T2>& v2,
					   const mem::smart_ptr<T3>& v3,
					   const mem::smart_ptr<T4>& v4,
					   const mem::smart_ptr<T5>& v5
	)
    {
	core::MultipleValues& me = *(core::lisp_multipleValues());
	SUPPRESS_GC();
        me.setMaxSize();
        me.valueSet(1, v1);
	me.valueSet(2, v2);
	me.valueSet(3, v3);
	me.valueSet(4, v4);
	me.valueSet(5, v5);
	me.setSize(6);
        ENABLE_GC();
        return mem::multiple_values<T0>(v0,6);
    }


    template <class T0,class T1,class T2,class T3, class T4>
    static mem::multiple_values<T0> Values(const mem::smart_ptr<T0>& v0,
					   const mem::smart_ptr<T1>& v1,
					   const mem::smart_ptr<T2>& v2,
					   const mem::smart_ptr<T3>& v3,
					   const mem::smart_ptr<T4>& v4
	)
    {
	core::MultipleValues& me = *(core::lisp_multipleValues());
	SUPPRESS_GC();
        me.setMaxSize();
        me.valueSet(1, v1);
	me.valueSet(2, v2);
	me.valueSet(3, v3);
	me.valueSet(4, v4);
	me.setSize(5);
        ENABLE_GC();
        return mem::multiple_values<T0>(v0,5);
    }



    template <class T0,class T1,class T2,class T3>
    static mem::multiple_values<T0> Values(const mem::smart_ptr<T0>& v0,
					   const mem::smart_ptr<T1>& v1,
					   const mem::smart_ptr<T2>& v2,
					   const mem::smart_ptr<T3>& v3
	)
    {
	core::MultipleValues& me = *(core::lisp_multipleValues());
	SUPPRESS_GC();
        me.setMaxSize();
        me.valueSet(1, v1);
	me.valueSet(2, v2);
	me.valueSet(3, v3);
	me.setSize(4);
        ENABLE_GC();
        return mem::multiple_values<T0>(v0,4);
    }



    template <class T0,class T1,class T2>
    static mem::multiple_values<T0> Values(const mem::smart_ptr<T0>& v0,
					   const mem::smart_ptr<T1>& v1,
					   const mem::smart_ptr<T2>& v2
	)
    {
	core::MultipleValues& me = *(core::lisp_multipleValues());
	SUPPRESS_GC();
        me.setMaxSize();
        me.valueSet(1, v1);
	me.valueSet(2, v2);
	me.setSize(3);
        ENABLE_GC();
        return mem::multiple_values<T0>(v0,3);
    }



    template <class T0,class T1>
    static mem::multiple_values<T0> Values(const mem::smart_ptr<T0>& v0,
					   const mem::smart_ptr<T1>& v1)
    {
	core::MultipleValues& me = *(core::lisp_multipleValues());
	SUPPRESS_GC();
        me.setMaxSize();
        me.valueSet(1, v1);
	me.setSize(2);
        ENABLE_GC();
        return mem::multiple_values<T0>(v0,2);
    }




    template <class T0>
    static mem::multiple_values<T0> Values(const mem::smart_ptr<T0>& v0)
    {
	core::MultipleValues& me = *(core::lisp_multipleValues());
        me.setSize(1);
	return mem::multiple_values<T0>(v0,1);
    }

	      

    template <class T0>
    static mem::multiple_values<T0> Values0()
    {
	core::MultipleValues& me = *(core::lisp_multipleValues());
        me.setSize(0);
	return mem::multiple_values<T0>(_Nil<T0>(),0);
    }








#if 0
#define DEFINE_RETURN_VALUE_TYPE(_ty_) template <> inline _ty_ Values1(_ty_ v0) { return v0;}
#else
#define DEFINE_RETURN_VALUE_TYPE(_ty_) 
#endif



#define MULTIPLE_VALUES_RETURN()
    /*! Does nothing, just notes that a call returns multiple values */






#define MULTIPLE_VALUES_CONTEXT()
//#define MULTIPLE_VALUES_SPECIAL_ACCESS(_var_) core::MultipleValues* _var_ = core::lisp_multipleValues()



#define RET(_x_x_x_) return(Values1(_x_x_x_))

#define RET_POD(_x_x_x_) return(_x_x_x_)

#define RET_REF(_x_x_x_) return(_x_x_x_)

#define RET_PASS_THROUGH(_t_t_t_) return(Values_pass_through<_t_t_t_>())

#endif /* _core_MultipleValues_H */


