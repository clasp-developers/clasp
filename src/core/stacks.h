#ifndef _core_stacks_H_
#define _core_stacks_H_

#include "core/foundation.h"
#include "core/sourceFileInfo.fwd.h"
#include "core/stacks.fwd.h"

namespace core
{

/*! Put this macro anywhere in the C++ code and it will update the
  current source line number in the InvocationHistoryStack
  to the current __LINE__ in the current C++ source - this is done within the _G() macro */
//#define	_LINE() {::core::_threadIHS.top()->setLineNumberColumnForCxxFunction(__LINE__,0,__FUNCTION__);}

    class InvocationHistoryStack;


#pragma GCC visibility push(default)
    class InvocationHistoryFrame //: public gctools::StackRoot
    {
	friend class InvocationHistoryStack;
    public:
	static const int NoLine = -1;
	static const int NoColumn = -1;
    public:
	uint			_Index;
	InvocationHistoryStack* _Stack;
	InvocationHistoryFrame*	_Next;
	int			_Bds;
	Closure* 	        closure;
        ActivationFrame_sp      environment;
        int                     runningSourceFileInfoHandle;
        int                     runningLineNumber;
        int                     runningColumn;
    public:
	InvocationHistoryFrame(Closure* fc, ActivationFrame_sp env=_Nil<ActivationFrame_O>());
	InvocationHistoryFrame(int sourceFileInfoHandle, int lineno, int column, ActivationFrame_sp env=_Nil<ActivationFrame_O>());
	ATTR_WEAK virtual ~InvocationHistoryFrame();
	InvocationHistoryFrame* next() { return this->_Next;};
	uint index() { return this->_Index;};
	virtual string sourcePathName() const;
	virtual int lineNumber() const { return this->runningLineNumber; };
	virtual int column() const { return this->runningColumn; };
	virtual void setSourcePos(int fileHandle, uint lineNumber, uint column) {
            this->runningSourceFileInfoHandle = fileHandle;
            this->runningLineNumber = lineNumber;
            this->runningColumn = column;
        };
	virtual void setActivationFrame(ActivationFrame_sp af) { this->environment = af; };
	virtual string asString();
	string asStringLowLevel(Closure* closure,
                                const string& functionName,
				const string& sourceFileName,
				uint lineNumber, uint column ) const;

	virtual ActivationFrame_sp activationFrame() const { return this->environment; };
	virtual int bds() const {return this->_Bds;};
    public:
    };


#pragma GCC visibility pop




    class InvocationHistoryStack
    {
    private:
	InvocationHistoryFrame*	_Top;

    public:
	InvocationHistoryStack() : _Top(NULL) {};

	InvocationHistoryFrame* top() const { return this->_Top;};
	void push(InvocationHistoryFrame* frame)
	{
	    this->_Top = frame;
	}

	void pop()
	{
	    if ( this->_Top!=NULL )
	    {
		this->_Top = this->_Top->next();
	    }
	}

	uint size()
	{
	    uint count = 0;
	    InvocationHistoryFrame* cur = this->_Top;
	    while ( cur )
	    {
		++count;
		cur = cur->next();
	    }
	    return count;
	}

	void setExpressionForTop(T_sp expression);

	void setSourcePosForTop(int sourceFileHandle, uint lineNumber, uint column)
	{
	    if ( this->_Top==NULL )
	    {
		printf("%s:%d IHSTop must never be NULL!\n", __FILE__, __LINE__);
		exit(1);
	    }
	    this->_Top->setSourcePos(sourceFileHandle,lineNumber,column);
	}

	void setActivationFrameForTop(ActivationFrame_sp af)
	{
	    if ( this->_Top==NULL )
	    {
		printf("%s:%d IHSTop must never be NULL!\n", __FILE__, __LINE__);
		exit(1);
	    }
	    this->_Top->setActivationFrame(af);
	}
	    
	void setSourcePosForTop(int sourceFileHandle, uint lineNumber)
	{
	    this->_Top->setSourcePos(sourceFileHandle,lineNumber,0);
	}

	vector<InvocationHistoryFrame*> asVectorFrames();

	string asString() const;


    };






    class DynamicBinding
    {
    public:
	Symbol_sp 	_Var;
	T_sp 		_Val;
	DynamicBinding(Symbol_sp sym, T_sp val) : _Var(sym), _Val(val) {};
    };

#pragma GCC visibility push(default)
    class DynamicBindingStack
    {
    public:
        gctools::Vec0<DynamicBinding> 	_Bindings;
    public:
	int top() const { return this->_Bindings.size()-1; }

	Symbol_sp topSymbol() const { return this->_Bindings.back()._Var;};

	Symbol_sp var(int i) const { return this->_Bindings[i]._Var;};
	T_sp val(int i) const { return this->_Bindings[i]._Val;};
	
	ATTR_WEAK void push(Symbol_sp var);
	ATTR_WEAK void pop();

	void reserve(int x) { this->_Bindings.reserve(x);};

	int size() const { return this->_Bindings.size(); };

    };	
#pragma GCC visibility pop
	

    
    InvocationHistoryFrame* get_ihs_ptr(int idx);
    T_sp af_ihsBacktrace(T_sp outDesignator, T_sp msg);
    int af_ihsTop();
    void af_ihsTopSetLineColumn(int lineno, int column);
    int af_ihsPrev(int idx);
    int af_ihsNext(int idx);
    Function_sp af_ihsFun(int idx);
    Environment_sp af_ihsEnv(int idx);
    /*! Return the current frame index stored in core:*ihs-current*
      Update core:*ihs-current* to a valid value */
    int af_ihsCurrentFrame();
    /*! Set the current core:*ihs-current* value.
      If the idx is out of bounds then return a valid value */
    int af_setIhsCurrentFrame(int idx);
    

}


namespace core {

    /*! Exception stack information */

    typedef enum { NullFrame, CatchFrame, BlockFrame, TagbodyFrame } FrameKind;
    /*! Store the information for the exception 
      For CatchThrow:   _Obj1
    */
    class ExceptionEntry {
    public:
        ExceptionEntry() : _FrameKind(NullFrame)
                         , _Key(_Nil<T_O>()) {};
        ExceptionEntry(FrameKind k, T_sp key) : _FrameKind(k)
                                                  , _Key(key) {};
        FrameKind       _FrameKind;
        T_sp            _Key;
    };


    class ExceptionStack {
        FRIEND_GC_SCANNER();
    private:
        gctools::Vec0<ExceptionEntry>   _Stack;
    public:
        ExceptionEntry& operator[](int i) { return this->_Stack[i];};
        size_t size() const { return this->_Stack.size();};

        int push(FrameKind kind, T_sp key) {
            int frame = this->_Stack.size();
            this->_Stack.emplace_back(kind,key);
            return frame;
        }
        void pop() {
            this->_Stack.pop_back();
        };
        /*! Return the index of the stack entry with the matching key.
          If return -1 then the key wasn't found */
        int findKey(FrameKind kind, T_sp key);
        T_sp backKey() const {return this->_Stack.back()._Key; };
        void unwind(size_t newTop) { this->_Stack.resize(newTop); };
    };


};



#define INVOCATION_HISTORY_FRAME() core::InvocationHistoryFrame zzzFrame(this);

namespace core{
    void initialize_stacks();
};



#endif /* _core_stacks_H_ */
