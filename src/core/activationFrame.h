#ifndef	core_ActivationFrame_H
#define	core_ActivationFrame_H    

//#define DEBUG_FRAME



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
	static string clasp_asString(ActivationFrame_sp af);
    public:
	ActivationFrame_O() : Base() {};
	virtual ~ActivationFrame_O() {};

	virtual T_sp* argArray() {SUBIMP();};


	virtual T_sp& operator[](int idx) {SUBIMP();};
	virtual const T_sp& operator[](int idx) const{SUBIMP();};

	virtual Environment_sp currentVisibleEnvironment() const;
	virtual ActivationFrame_sp getActivationFrame() const;


	virtual T_sp _lookupValue(int depth, int index) const;
	virtual const T_sp& lookupValueReference(int depth, int index) const;
	virtual Function_sp _lookupFunction(int depth, int index) const;
	virtual T_sp _lookupTagbodyId(int depth, int index) const;


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



	T_sp _lookupValue(int depth, int index) const;
	const T_sp& lookupValueReference(int depth, int index) const;

	virtual bool _updateValue(Symbol_sp sym, T_sp obj );
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
	    FunctionFrame_sp vf(FunctionFrame_O::create(cl_length(args),parent));
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


	virtual Function_sp _lookupFunction(int depth, int index) const;

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

        T_sp _lookupTagbodyId(int depth, int index) const;

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




namespace frame
{
    typedef core::T_O*  ElementType;
    typedef gctools::smart_ptr<core::STACK_FRAME>     FrameType;
        
    static const size_t IdxNumElements = 0;
    static const size_t IdxParent = 1;
    static const size_t IdxDebugInfo = 2;
    static const size_t IdxValuesArray = 3;

    /*! Calculate the number of elements required to represent the frame.
     It's IdxValuesArray+#elements */
    inline size_t FrameSize(size_t elements) {
        return elements+IdxValuesArray;
    }

    /*! Return the start of the values array */
    inline ElementType* ValuesArray(FrameType f) {
        core::T_O** frameImpl(gctools::tagged_ptr<core::STACK_FRAME>::untagged_frame(f.px));
        return &frameImpl[IdxValuesArray];
    }

    inline size_t ValuesArraySize(core::T_O** frameImpl) { return gctools::tagged_ptr<core::T_O>::untagged_fixnum(frameImpl[IdxNumElements]);};

    inline core::T_sp DebugInfo(FrameType f)
    {
        core::T_O** frameImpl(gctools::tagged_ptr<core::STACK_FRAME>::untagged_frame(f.px));
        return gctools::smart_ptr<core::T_O>(frameImpl[IdxDebugInfo]);
    }

    inline void SetDebugInfo(FrameType f, core::T_sp debugInfo) {
        core::T_O** frameImpl(gctools::tagged_ptr<core::STACK_FRAME>::untagged_frame(f.px));
        frameImpl[IdxDebugInfo] = debugInfo.as<core::T_O>().asTPtr();
    };

    inline void InitializeStackValueFrame(core::T_O** frameImpl, size_t sz, core::T_sp parent=_Nil<core::T_O>())
    {
#ifdef DEBUG_FRAME
        printf("%s:%d InitializeStackValueFrame @%p sz=%zu\n", __FILE__, __LINE__, frameImpl, sz );
#endif
        frameImpl[IdxNumElements] = gctools::tagged_ptr<core::T_O>::make_tagged_fixnum(sz);
        frameImpl[IdxParent] = parent.asTPtr();
        frameImpl[IdxDebugInfo] = gctools::tagged_ptr<core::T_O>::make_tagged_nil();
        for ( size_t i(IdxValuesArray), iEnd(IdxValuesArray+sz); i<iEnd; ++i ) {
            frameImpl[i] = gctools::tagged_ptr<core::T_O>::make_tagged_unbound();
        }
    };

    inline void SetParentFrame(core::T_sp f, core::T_sp parent)
    {
        core::T_O** frameImpl(gctools::tagged_ptr<core::STACK_FRAME>::untagged_frame(f.px));
        frameImpl[IdxParent] = parent.asTPtr();
    }
        
    inline gctools::smart_ptr<core::T_O> ParentFrame(core::T_O** frameImpl)
    {
        return gctools::smart_ptr<core::T_O>(frameImpl[IdxParent]);
    }
    inline gctools::smart_ptr<core::T_O> ParentFrame(core::T_sp f)
    {
        core::T_O** frameImpl(gctools::tagged_ptr<core::STACK_FRAME>::untagged_frame(f.px));
        return ParentFrame(frameImpl);
    }

    inline gctools::smart_ptr<core::T_O> Lookup(core::T_sp f, int idx)
    {
        core::T_O** frameImpl(gctools::tagged_ptr<core::STACK_FRAME>::untagged_frame(f.px));
        return gctools::smart_ptr<core::T_O>(frameImpl[idx+IdxValuesArray]);
    }

    inline core::T_sp LookupTagbodyId(core::T_sp f, int idx)
    {
        IMPLEMENT_MEF(BF("I don't currently support LookupTagbodyId for tagged_frame"));
#if 0   // If I supported this it might look like this
        core::T_O** frameImpl(gctools::tagged_ptr<core::STACK_FRAME>::untagged_frame(f.px));
        return gctools::smart_ptr<core::T_O>(frameImpl[idx+IdxValuesArray]);
#endif
    }

    inline gctools::smart_ptr<core::T_O> Value(core::T_O** frameImpl, int idx) {
        return gctools::smart_ptr<core::T_O>(frameImpl[idx+IdxValuesArray]);
    }

    inline int countFunctionContainerEnvironments(core::T_sp f) {
        core::T_O** frameImpl(gctools::tagged_ptr<core::STACK_FRAME>::untagged_frame(f.px));
        return core::Environment_O::clasp_countFunctionContainerEnvironments(frameImpl[IdxParent]);
    }


    inline bool findValue(core::T_sp f, core::Symbol_sp sym, int& depth, int& index, bool& special, core::T_sp& value )
    {
        core::T_O** frameImpl(gctools::tagged_ptr<core::STACK_FRAME>::untagged_frame(f.px));
	if ( gctools::tagged_ptr<core::T_O>::tagged_nilp(frameImpl[IdxDebugInfo]) ) {
            ++depth;
            return core::Environment_O::clasp_findValue(gctools::smart_ptr<core::T_O>(frameImpl[IdxParent]),sym,depth,index,special,value);
	}
        core::T_sp debugInfo = gctools::smart_ptr<core::T_O>(frameImpl[IdxDebugInfo]);
        if ( lisp_search(debugInfo,sym,index) ) {
            value = frameImpl[index+IdxValuesArray];
            return true;
        }
	++depth;
	return core::Environment_O::clasp_findValue(ParentFrame(frameImpl),sym,depth,index,special,value);
    }



    inline bool findFunction(core::T_sp f, core::T_sp functionName, int& depth, int& index, core::Function_sp& func)
    {
        core::T_O** frameImpl(gctools::tagged_ptr<core::STACK_FRAME>::untagged_frame(f.px));
        core::T_sp parent = core::Environment_O::clasp_currentVisibleEnvironment(ParentFrame(frameImpl));
	++depth;
	return core::Environment_O::clasp_findFunction(parent,functionName,depth,index,func);
    }


    inline bool findTag(core::T_sp f, core::Symbol_sp sym, int& depth, int& index )
    {_G();
        core::T_O** frameImpl(gctools::tagged_ptr<core::STACK_FRAME>::untagged_frame(f.px));
        core::T_sp parent = core::Environment_O::clasp_currentVisibleEnvironment(ParentFrame(frameImpl));
        ++depth;
	return core::Environment_O::clasp_findTag(parent,sym,depth,index);
    }



    inline string SummaryOfContents(core::T_sp env)
    {
        return "tagged_frame contents here";
    }
#if 0
    string ValueFrame_O::summaryOfContents() const
    {
	stringstream ss;
	ss << "---" << this->_instanceClass()->classNameAsString() << "#" << this->environmentId() << " :len " << this->length() << std::endl;
	Vector_sp debuggingInfo = _Nil<Vector_O>();
	if ( this->_DebuggingInfo.notnilp() )
	{
	    debuggingInfo = this->_DebuggingInfo.as<Vector_O>();
	}
	for ( int i=0; i<this->_Objects.capacity(); ++i )
	{
	    if ( debuggingInfo.notnilp() && (i < cl_length(debuggingInfo)) )
	    {
		ss << _rep_(debuggingInfo->elt(i)) << " ";
	    } else
	    {
		ss << ":arg"<<i<<"@" << (void*)(&(this->operator[](i))) << " ";
	    }
	    if ( !this->operator[](i) )
	    {
		ss << "UNDEFINED";
	    } else if ( !this->boundp_entry(i) )
	    {
		ss << "!!UNBOUND!! ";
	    } else
	    {
		if ( af_activation_frame_p(this->operator[](i)) )
		{
		    ss << "ActivationFrame@"<< (void*)(&(this->operator[](i)));
		} else
		{
		    ss << _rep_(this->operator[](i)) << "  ";
		}
	    }
	    ss << std::endl;
	}
	return((ss.str()));
    }

#endif



    inline bool UpdateValue(core::T_sp f, core::Symbol_sp sym, core::T_sp obj)
    {
        core::T_O** frameImpl(gctools::tagged_ptr<core::STACK_FRAME>::untagged_frame(f.px));
        if ( gctools::tagged_ptr<core::T_O>::tagged_nilp(frameImpl[IdxDebugInfo]) ) {
	    return core::af_updateValue(ParentFrame(frameImpl),sym,obj);
	}
        core::T_sp debugInfo = gctools::smart_ptr<core::T_O>(frameImpl[IdxDebugInfo]);
        int index;
        if ( lisp_search(debugInfo,sym,index) ) {
            frameImpl[IdxValuesArray+index] = obj.asTPtr();
            return true;
        }
        return core::af_updateValue(ParentFrame(frameImpl),sym,obj);
    }

};


#if 1
#define ALLOC_STACK_VALUE_FRAME(frameImpl,frame,numValues)     \
    frame::ElementType frameImpl[frame::FrameSize(numValues)]; \
    gctools::smart_ptr<core::STACK_FRAME> frame(frameImpl);    \
    frame::InitializeStackValueFrame(frameImpl,numValues)
#else
#define ALLOC_STACK_VALUE_FRAME(frameImpl,frame,numValues)     \
    frame::ElementType* frameImpl = (frame::ElementType*)(alloca(sizeof(frame::ElementType)*frame::FrameSize(numValues))); \
    gctools::smart_ptr<core::STACK_FRAME> frame(frameImpl);    \
    frame::InitializeStackValueFrame(frameImpl,numValues)
#endif


#endif
