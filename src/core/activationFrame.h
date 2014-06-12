#ifndef	core_ActivationFrame_H
#define	core_ActivationFrame_H    

#include <utility>
#include 	"foundation.h"
#include 	"object.h"
#include	"activationFrame.fwd.h"
#include 	"core/loadTimeValues.fwd.h"
#include	"environment.h"
#include	"holder.h"



// may need more later
//#include GC_INTERFACE_HEADER



namespace core
{


    /*! Types of ActivationFrame */
//    typedef enum { variables_frame=0, functions_frame=1, block_frame=2, tagbody_frame=3 } ActivationFrameType;

    // TODO: ActivationFrame should adopt the _findValue behaviors of RuntimeVisibleEnvironment 
    // TODO: and it should inherit from Environment_O not RuntimeVisibleEnvironment_O
    class ActivationFrame_O : public Environment_O // RuntimeVisibleEnvironment_O
    {
	LISP_BASE1( Environment_O ); // RuntimeVisibleEnvironment_O);
	LISP_VIRTUAL_CLASS(core,CorePkg,ActivationFrame_O,"ActivationFrame");
    protected:
    public:
	static string nilCheck_asString(ActivationFrame_sp af);
    public:
	ActivationFrame_O() : Base() {};
	virtual ~ActivationFrame_O() {};

	virtual T_sp* argArray() {SUBIMP();};


	virtual T_sp& operator[](int idx) {SUBIMP();};
	virtual const T_sp& operator[](int idx) const{SUBIMP();};

	virtual Environment_sp currentVisibleEnvironment() const;
	virtual ActivationFrame_sp getActivationFrame() const;


	virtual T_sp lookupValue(int depth, int index) const;
	virtual const T_sp& lookupValueReference(int depth, int index) const;
	virtual T_sp lookupFunction(int depth, int index) const;
	virtual T_sp lookupTagbodyId(int depth, int index) const;


	virtual bool _findTag(Symbol_sp tag, int& depth, int& index) const;
	virtual bool _findValue(Symbol_sp sym, int& depth, int& index, bool& special, T_sp& value) const;
	virtual bool _findFunction(T_sp functionName, int& depth, int& index, Function_sp& value) const;


	virtual ActivationFrame_sp& parentFrameRef() { SUBIMP(); };
	virtual ActivationFrame_sp parentFrame() const { SUBIMP(); };

	/*! Methods for interogating ActivationFrames as Environments */
	Environment_sp getParentEnvironment() const { return this->parentFrame(); };
	/*! Method for interogating ActivationFrames as Environments */
	virtual string summaryOfContents() const;

	void setParentFrame(ActivationFrame_sp parent) { this->parentFrameRef() = parent;};
	/*! Return the number of arguments */
	virtual uint length() const {SUBIMP();};

	virtual bool boundp_entry(uint idx) const {SUBIMP();}

	/*! Set one entry of the activation frame */
	virtual void set_entry(uint idx, T_sp obj) {SUBIMP();}

	virtual T_sp entry(int idx) const {SUBIMP();}
	virtual const T_sp& entryReference(int idx) const {SUBIMP();}

    private:
	virtual string asString() const;
    public:
	virtual string __repr__() const { return this->asString();};
	virtual void dump() { string ts = this->asString(); printf("%s\n", ts.c_str() );};

	/*! Access a function */
	virtual Function_sp function(int idx) const {THROW_HARD_ERROR(BF("Subclass must implement function(idx)"));};

	Cons_sp asCons(int start=0) const
	{_G();
	    Cons_sp dummy = Cons_O::create(_Nil<T_O>());
	    Cons_sp cur = dummy;
	    for ( int i=start; i<(int)this->length(); i++ )
	    {
		Cons_sp one = Cons_O::create(this->entry(i));
		cur->setCdr(one);
		cur = one;
	    }
	    return cCdr(dummy);
	}

    }; // ActivationFrame class
}; // core namespace

template<> struct gctools::GCInfo<core::ValueFrame_O> {
    static bool const NeedsInitialization = false;
    static bool const NeedsFinalization = false;
    static bool const Moveable = true;
    static bool constexpr Atomic = false;
};
TRANSLATE(core::ActivationFrame_O);




namespace core
{
    class ValueFrame_O : public ActivationFrame_O
    {
	LISP_BASE1(ActivationFrame_O);
	LISP_CLASS(core,CorePkg,ValueFrame_O,"ValueFrame");
    protected:
	ActivationFrame_sp	        _ParentFrame;
        gctools::Frame0<T_sp>                 _Objects;
//IndirectObjectArray             _Objects;
	core::T_sp 			_DebuggingInfo;
    public:

	static ValueFrame_sp createForArgArray(size_t nargs, T_sp* argArray, const ActivationFrame_sp& parent)
	{_G();
            GC_ALLOCATE(ValueFrame_O,vf);
            vf->allocate(nargs);
            // TODO: This is used for all generic function calls - is there a better way than copying the ValueFrame??????
            for ( int i(0); i<nargs; ++i ) {
                vf->_Objects[i] = argArray[i];
            }
            vf->_ParentFrame = parent;
#if 0
	    vf->_OwnArgs = false;
	    vf->_NumArgs = nargs;
	    vf->_Args = argArray;
#endif
	    return vf;
	}
	static ValueFrame_sp create(const ActivationFrame_sp& parent)
	{_G();
	    GC_ALLOCATE(ValueFrame_O,vf);
            vf->_ParentFrame = parent;
	    return vf;
	}

	static ValueFrame_sp create(int numArgs,const ActivationFrame_sp& parent)
	{_G();
            GC_ALLOCATE(ValueFrame_O,vf);
            vf->_ParentFrame = parent;
            vf->allocate(numArgs);
//	    vf->allocateStorage(numArgs);
	    return vf;
	}



	static ValueFrame_sp create(Cons_sp values,ActivationFrame_sp parent);

	static ValueFrame_sp createFromReversedCons(Cons_sp values,ActivationFrame_sp parent);

	static ValueFrame_sp createForLambdaListHandler(LambdaListHandler_sp llh,ActivationFrame_sp parent);

	template <class ... ARGS>
	static ValueFrame_sp create_fill_args(ActivationFrame_sp parent, ARGS&&... args)
	{_G();
            GC_ALLOCATE(ValueFrame_O,vf);
            vf->_ParentFrame = parent;
            vf->allocate(0,std::forward<ARGS>(args)...);
	    return vf;
	}


	template <class ... ARGS>
	static ValueFrame_sp create_fill_numExtraArgs(int numExtraArgs, ActivationFrame_sp parent, ARGS&&... args)
	{_G();
            GC_ALLOCATE(ValueFrame_O,vf);
            vf->_ParentFrame = parent;
            vf->allocate(numExtraArgs,std::forward<ARGS>(args)...);
	    return vf;
	}




	ValueFrame_O() : Base(), _Objects(), _DebuggingInfo(_Nil<T_O>()) {
        };


#if 0
        /*! ValueFrames must always be initialized with _Unbound !!!!! */
        template <class...ARGS>
        ValueFrame_O(size_t numExtraArgs, ARGS&&...args) : _Objects(numExtraArgs,_Unbound<T_O>(),std::forward<ARGS>(args)...), _DebuggingInfo(_Nil<T_O>())
        {
//            printf("%s::%d ctor ValueFrame@%p\n", __FILE__, __LINE__, this);
        };
#endif

	virtual ~ValueFrame_O() {
//            printf("%s::%d dtor ValueFrame@%p\n", __FILE__, __LINE__, this);
        };

    private:
        template <class...ARGS>
        void allocate(size_t numExtraArgs, ARGS&&...args)
        {
            this->_Objects.allocate(numExtraArgs,_Unbound<T_O>(),std::forward<ARGS>(args)...);
        };

    public:

	virtual ActivationFrame_sp& parentFrameRef() { return this->_ParentFrame; };
	virtual ActivationFrame_sp parentFrame() const { return this->_ParentFrame; };

	void attachDebuggingInfo(core::T_sp debuggingInfo)
	{_G();
	    this->_DebuggingInfo = debuggingInfo;
	}

	core::T_sp debuggingInfo() const
	{
	    return this->_DebuggingInfo;
	}


    public:
	virtual T_sp& operator[](int idx);
	virtual const T_sp& operator[](int idx) const;

	T_sp* argArray() { return this->_Objects.data(); };

	/*! Return the number of arguments */
	virtual uint length() const { return this->_Objects.capacity(); };



	T_sp lookupValue(int depth, int index) const;
	const T_sp& lookupValueReference(int depth, int index) const;

	virtual bool updateValue(Symbol_sp sym, T_sp obj );
	virtual bool _findValue(Symbol_sp sym, int& depth, int& index, bool& special, T_sp& value) const;



	bool boundp_entry(uint idx) const
	{
            return !this->_Objects[idx].unboundp();
        }

	/*! Set one entry of the activation frame */
	void set_entry(uint idx, T_sp obj)
	{_G();
            this->_Objects[idx] = obj;
	}

	T_sp entry(int idx) const
	{_G();
            return this->_Objects[idx];
	}

#if 0
	virtual T_sp& entryReference(int idx) const
	{_G();
            return this->_Objects.entryReference(idx);
	}
#endif
	/*! Fill the activation frame starting at entry istart with values.
	  DO NOT OVERFLOW THE ValueFrame!!!! */
	void fillRestOfEntries(int istart, Cons_sp values);

	/*! Method for interogating ActivationFrames as Environments */
	virtual string summaryOfContents() const;

	string asString() const;
    };





};


template<> struct gctools::GCInfo<core::FunctionFrame_O> {
    static bool const NeedsInitialization = false;
    static bool const NeedsFinalization = false;
    static bool const Moveable = true;
    static bool constexpr Atomic = false;
};

namespace core {
    class FunctionFrame_O : public ActivationFrame_O
    {
	LISP_BASE1(ActivationFrame_O);
	LISP_CLASS(core,CorePkg,FunctionFrame_O,"FunctionFrame");
    private:
	ActivationFrame_sp	_ParentFrame;
        gctools::Frame0<T_sp>                 _Objects;
//        IndirectObjectArray     _Objects;
    public:
#if 0
	inline void allocateStorage(int numArgs)
	{_G();
	    this->_Objects.allocateStorage(numArgs);
	}
#endif
    private:
        template <class...ARGS>
        void allocate(size_t numExtraArgs, ARGS&&...args)
        {
            this->_Objects.allocate(numExtraArgs,_Unbound<T_O>(),std::forward<ARGS>(args)...);
        };
    public:
	static FunctionFrame_sp create(ActivationFrame_sp parent)
	{_G();
	    GC_ALLOCATE(FunctionFrame_O,vf);
            vf->_ParentFrame = parent;
	    return vf;
	}

	static FunctionFrame_sp create(int numArgs,ActivationFrame_sp parent)
	{_G();
	    GC_ALLOCATE(FunctionFrame_O,vf);
            vf->_ParentFrame = parent;
            vf->allocate(numArgs);
	    return vf;
	}

	static FunctionFrame_sp create(Cons_sp args,ActivationFrame_sp parent)
	{_G();
	    FunctionFrame_sp vf(FunctionFrame_O::create(af_length(args),parent));
//	    vf->allocateStorage(args->length());
	    int idx = 0;
	    for ( core::Cons_sp cur = args; cur.notnilp(); cur=cCdr(cur) )
	    {
		vf->operator[](idx) = oCar(cur);// n e w (&(vf->_Args[idx])) T_sp(oCar(cur));
		++idx;
	    }
	    return vf;
	}


	template <class ... ARGS>
	static FunctionFrame_sp create_fill(ActivationFrame_sp parent, ARGS&&... args)
	{_G();
	    GC_ALLOCATE(FunctionFrame_O,vf);
            vf->_ParentFrame = parent;
            vf->allocate(0,std::forward<ARGS>(args)...);
	    return vf;
	}

        FunctionFrame_O() : Base(), _Objects() {};


        /*! FunctionFrames must always be initialized with _Unbound !!!!! */
#if 0
        template <typename...ARGS>
	FunctionFrame_O(size_t numExtraArgs, ARGS...args)
            : _Objects(numExtraArgs,_Unbound<T_O>(),args...), Base() {};
#endif
	virtual ~FunctionFrame_O() {}

    public:
	virtual ActivationFrame_sp& parentFrameRef() { return this->_ParentFrame; };
	virtual ActivationFrame_sp parentFrame() const { return this->_ParentFrame; };

	/*! Return the number of arguments */
	virtual uint length() const { return this->_Objects.capacity(); };
//	T_sp* argArray() { return this->_Objects.argArray(); };

	bool boundp_entry(uint idx) const
	{_G();
            return !this->_Objects[idx].unboundp();
	}

	/*! Set one entry of the activation frame */
	void set_entry(uint idx, T_sp obj)
	{_G();
            this->_Objects[idx] = obj;
	}
	T_sp entry(int idx) const;   // Return by reference for efficiency?
	const T_sp& entryReference(int idx) const;   // Return by reference for efficiency?

	string asString() const;

	/*! Method for interogating ActivationFrames as Environments */
	virtual string summaryOfContents() const;


	virtual T_sp lookupFunction(int depth, int index) const;

    };






};




namespace core
{
    class TagbodyFrame_O : public ActivationFrame_O
    {
	LISP_BASE1(ActivationFrame_O);
	LISP_CLASS(core,CorePkg,TagbodyFrame_O,"TagbodyFrame");
    private:
	ActivationFrame_sp	_ParentFrame;
    public:
	static TagbodyFrame_sp create(ActivationFrame_sp parent);

	virtual ActivationFrame_sp& parentFrameRef() { return this->_ParentFrame; };
	virtual ActivationFrame_sp parentFrame() const { return this->_ParentFrame; };
	virtual string summaryOfContents() const;

        T_sp lookupTagbodyId(int depth, int index) const;

	TagbodyFrame_O() : Base() {};
	virtual ~TagbodyFrame_O() {};

    public:
	string asString() const;
    };
};
template<> struct gctools::GCInfo<core::TagbodyFrame_O> {
    static bool const NeedsInitialization = false;
    static bool const NeedsFinalization = false;
    static bool const Moveable = true;
    static bool constexpr Atomic = false;
};





#endif
