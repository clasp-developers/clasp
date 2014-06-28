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

    typedef enum { TopLevel, Debugger, CxxFunction, LispInterpretedFunction, LispCompiledFunction, MacroExpansionFunction } IHFLeafKind;


#pragma GCC visibility push(default)
    class InvocationHistoryFrame //: public gctools::StackRoot
    {
	friend class InvocationHistoryStack;
    public:
	static const int NoLine = -1;
	static const int NoColumn = -1;
    protected:
	IHFLeafKind		_IHSFrameKind;
	uint			_Index;
	InvocationHistoryStack* _Stack;
	InvocationHistoryFrame*	_Next;
	bool			_WroteToLog;
	int			_Bds;
    public:
	InvocationHistoryFrame(IHFLeafKind kind, InvocationHistoryStack& stack);
	ATTR_WEAK virtual ~InvocationHistoryFrame();
	InvocationHistoryFrame* next() { return this->_Next;};
	uint index() { return this->_Index;};
	virtual string sourcePathName() const = 0;
	virtual int lineNumber() const = 0;
	virtual int column() const = 0;
	virtual void setLineNumberColumn(uint lineNumber, uint column) = 0;
	virtual void setLineNumberColumnForCxxFunction(uint lineNumber, uint column, const char* functionName);
	virtual void setActivationFrame(ActivationFrame_sp af) = 0;
	virtual string asString() const=0;
	virtual string typeName() const=0;
	string asStringLowLevel(const string& type, const string& functionName,
				const string& sourceFileName,
				uint lineNumber, uint column ) const;

	virtual Function_sp function() const =0;
	virtual ActivationFrame_sp activationFrame() const =0;
	virtual int bds() const {return this->_Bds;};
    public:
    };


    class ATTR_WEAK TopLevelIHF : public InvocationHistoryFrame
    {
	friend class InvocationHistoryStack;
        typedef InvocationHistoryFrame Base;
    protected:
	SourceFileInfo_sp 	_SourceFileInfo;
	ActivationFrame_sp 	_ActivationFrame;
	uint 			_LineNumber;
	uint			_Column;
	uint			_FilePos;
    public:
        virtual void keyFunctionForVtable() ATTR_WEAK;
    public:
	TopLevelIHF(InvocationHistoryStack& stack,T_sp expression);
	ATTR_WEAK virtual ~TopLevelIHF() {};
	void setLineNumberColumn(uint lineNumber, uint column)
	{
	    this->_LineNumber = lineNumber;
	    this->_Column = column;
	}
	void setActivationFrame(ActivationFrame_sp af)
	{
	    this->_ActivationFrame = af;
	}
	virtual string asString() const;
	virtual string typeName() const { return " TopLevel";};
	virtual Function_sp function() const { return _Nil<Function_O>();};
	virtual ActivationFrame_sp activationFrame() const { return this->_ActivationFrame;};
	virtual string sourcePathName() const;
	virtual int lineNumber() const {return this->_LineNumber;};
	virtual int column() const {return this->_Column;};
    };
#pragma GCC visibility pop


    class DebuggerIHF : public InvocationHistoryFrame
    {
	friend class InvocationHistoryStack;
        typedef InvocationHistoryFrame Base;
    protected:
	ActivationFrame_sp 	_ActivationFrame;
    public:

	DebuggerIHF(InvocationHistoryStack& stack,ActivationFrame_sp af)
	    : InvocationHistoryFrame(Debugger,stack),
	      _ActivationFrame(af)
		  {};
	virtual ~DebuggerIHF() {};
	void setLineNumberColumn(uint lineNumber, uint column);
	void setActivationFrame(ActivationFrame_sp af);
	virtual ActivationFrame_sp activationFrame() const { return this->_ActivationFrame;};
	virtual string asString() const;
	virtual string typeName() const { return " Debugger";};
	virtual Function_sp function() const { return _Nil<Function_O>();};
	virtual string sourcePathName() const {return "";};
	virtual int lineNumber() const {return NoLine;};
	virtual int column() const {return NoColumn;};
    };



    class CxxFunctionIHF : public InvocationHistoryFrame
    {
	friend class InvocationHistoryStack;
        typedef  InvocationHistoryFrame Base;
    protected:
	Function_sp 	        _Function;
#if 0
	int			_Nargs;
	ArgArray		_Args;
#endif
	uint			_LineNumber;
    public:
    public:
	CxxFunctionIHF(InvocationHistoryStack& stack, Function_sp function ) : InvocationHistoryFrame(CxxFunction,stack)
                                                                             , _Function(function)
#if 0
                                                                             , _Nargs(0)
                                                                             , _Args(NULL)
#endif
                                                                             , _LineNumber(0) {};
#if 0    // comment out this constructor - it should never be called because we shouldn't support nargs/args
	CxxFunctionIHF(InvocationHistoryStack& stack, Function_sp function , int nargs, ArgArray args ) : InvocationHistoryFrame(CxxFunction,stack)
                                                                                                       , _Function(function)
                                                                                                       , _Nargs(nargs)
                                                                                                       , _Args(args)
                                                                                                       , _LineNumber(0) {};
#endif
	virtual ~CxxFunctionIHF();
//	virtual void setLineNumberColumnForCxxFunction(uint lineNumber, uint column, const char* functionName);

	void setLineNumberColumn(uint lineNumber, uint column)
	{
	    this->_LineNumber = lineNumber;
	}
	void setActivationFrame(ActivationFrame_sp af);
	ActivationFrame_sp activationFrame() const;
	virtual string typeName() const { return "      C++";};
	virtual string asString() const;
	virtual string sourcePathName() const;
	virtual int lineNumber() const {return this->_LineNumber;};
	virtual int column() const {return 1;};
	virtual Function_sp function() const { return this->_Function;};
    };


    class LispFunctionIHF : public InvocationHistoryFrame
    {
	friend class InvocationHistoryStack;
        typedef InvocationHistoryFrame Base;
    protected:
	Function_sp 		_Function;
	ActivationFrame_sp	_ActivationFrame;
	uint			_LineNumber;
	uint 			_Column;
    public:
    public:
	LispFunctionIHF(IHFLeafKind kind, InvocationHistoryStack& stack, Function_sp function);
	LispFunctionIHF(IHFLeafKind kind, InvocationHistoryStack& stack, Function_sp function, ActivationFrame_sp af);
	void setLineNumberColumn(uint lineNumber, uint column);
	void setActivationFrame(ActivationFrame_sp env);
	virtual ~LispFunctionIHF() {};
	virtual string asString() const =0;
	virtual string typeName() const=0;

	Function_sp function() const;
	ActivationFrame_sp activationFrame() const;
	virtual string sourcePathName() const;
	virtual int lineNumber() const {return this->_LineNumber;};
	virtual int column() const {return this->_Column;};
    };



    class LispInterpretedFunctionIHF : public LispFunctionIHF
    {
	friend class InvocationHistoryStack;
        typedef LispFunctionIHF Base;
    public:
    public:
	LispInterpretedFunctionIHF(InvocationHistoryStack& stack, Function_sp function, ActivationFrame_sp af )
	    : LispFunctionIHF(LispInterpretedFunction,stack, function, af ) {};
	virtual ~LispInterpretedFunctionIHF() {};
	virtual string asString() const;
	virtual string typeName() const { return "Interp.Lisp";};
    };


    class MacroExpansionIHF : public LispFunctionIHF
    {
	friend class InvocationHistoryStack;
        typedef LispFunctionIHF Base;
    public:
	MacroExpansionIHF(InvocationHistoryStack& stack, Function_sp function )
	    : LispFunctionIHF(MacroExpansionFunction,stack, function ) {};
	virtual ~MacroExpansionIHF() {};
	virtual string asString() const;
	virtual string typeName() const { return "Macro.Lisp";};
    };

    class LispCompiledFunctionIHF : public LispFunctionIHF
    {
	friend class InvocationHistoryStack;
    public:
	LispCompiledFunctionIHF(InvocationHistoryStack& stack, Function_sp function )
	    : LispFunctionIHF(LispCompiledFunction, stack, function) {};
	virtual ~LispCompiledFunctionIHF() {};
	virtual string asString() const;
	virtual string typeName() const { return "CompiledLisp";};

    };


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

	void setLineNumberColumnForTop(uint lineNumber, uint column)
	{
	    if ( this->_Top==NULL )
	    {
		printf("%s:%d IHSTop must never be NULL!\n", __FILE__, __LINE__);
		exit(1);
	    }
	    this->_Top->setLineNumberColumn(lineNumber,column);
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
	    
	void setLineNumberForTop(uint lineNumber)
	{
	    this->_Top->setLineNumberColumn(lineNumber,0);
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
        int findKey(FrameKind kind, T_sp key) {
            for ( int i(this->_Stack.size()-1); i>=0; --i ) {
                if (this->_Stack[i]._FrameKind == kind && this->_Stack[i]._Key == key) return i;
            }
            return -1;
        }
        T_sp backKey() const {return this->_Stack.back()._Key; };
        void unwind(size_t newTop) { this->_Stack.resize(newTop); };
    };


};


namespace core{
    void initialize_stacks();
};



#endif /* _core_stacks_H_ */
