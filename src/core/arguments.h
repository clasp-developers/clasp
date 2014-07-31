#ifndef	_core_arguments_H
#define _core_arguments_H

#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "symbol.h"

namespace core
{

#define SUB_LAMBDA_LIST		-3
#define	SPECIAL_TARGET		-2
#define	UNDEFINED_TARGET	-1
    class Argument
    {
    public:
	T_sp  		_ArgTarget;
	int		_ArgTargetFrameIndex;
	explicit Argument() : _ArgTarget(_Nil<T_O>())
                            , _ArgTargetFrameIndex(UNDEFINED_TARGET) {}
	explicit Argument(T_sp target) : _ArgTarget(target), _ArgTargetFrameIndex(UNDEFINED_TARGET) {};
        DECLARE_onHeapScanGCRoots();
	int targetFrameIndex() const
	{
	    return this->_ArgTargetFrameIndex;
	}
	void clear()
	{_G();
	    this->_ArgTarget = _Nil<T_O>();
	    this->_ArgTargetFrameIndex = UNDEFINED_TARGET;
	}
	Cons_sp classified() const;
	inline bool isDefined() const { return (this->_ArgTarget)&&(this->_ArgTarget.notnilp());};
	inline bool _symbolP() const { return af_symbolp(this->_ArgTarget); };
	Symbol_sp symbol() const;
	inline bool _lambdaListHandlerP() const { return af_lambda_list_handler_p(this->_ArgTarget); };
	LambdaListHandler_sp lambdaListHandler() const;
	inline bool _lambdaListP() const { return af_consP(this->_ArgTarget); };
	Cons_sp lambdaList() const;
	inline bool targetIsLexical() const { return this->_ArgTargetFrameIndex!=SPECIAL_TARGET;}
	virtual string asString() const;
    };
      

    class ArgumentWithDefault : public Argument
    {
    public:
	typedef Argument Base;
	T_sp 		_Default;
        ArgumentWithDefault() : _Default(_Nil<T_O>()) {};
	ArgumentWithDefault(T_sp target, T_sp def ) : Argument(target), _Default(def) {};
        DECLARE_onHeapScanGCRoots();
	string asString() const;
    };

    class RequiredArgument : public Argument
    {
    public:
	typedef Argument Base;
        RequiredArgument() {};
	RequiredArgument(T_sp target) : Argument(target) {};
	RequiredArgument(T_sp target, int frameIndex) : Argument(target) { this->_ArgTargetFrameIndex = frameIndex; };
        DECLARE_onHeapScanGCRoots();
	string asString() const;
    };



    class OptionalArgument : public ArgumentWithDefault
    {
    public:
	typedef ArgumentWithDefault Base;
	Argument	_Sensor;
        OptionalArgument() {};
	OptionalArgument(T_sp target, T_sp def, T_sp sensor) : ArgumentWithDefault(target,def), _Sensor(sensor) {};
        DECLARE_onHeapScanGCRoots();
	string asString() const;
    };


    class RestArgument : public Argument
    {
    public:
	typedef Argument Base;
	explicit RestArgument() : Argument() {};
	explicit RestArgument(T_sp target) : Argument(target) {};
        DECLARE_onHeapScanGCRoots();
	void setTarget(T_sp target) { this->_ArgTarget = target;};
	string asString() const;
    };

    class KeywordArgument : public ArgumentWithDefault
    {
    public:
	typedef ArgumentWithDefault Base;
	T_sp 		_Keyword;
	Argument	_Sensor;
        KeywordArgument() : ArgumentWithDefault(), _Keyword(_Nil<T_O>()), _Sensor() {};
	KeywordArgument(T_sp keyword, T_sp target, T_sp def, Symbol_sp sensor) : ArgumentWithDefault(target,def),_Keyword(keyword),_Sensor(sensor) {};
        DECLARE_onHeapScanGCRoots();
	string asString() const;
    };





    class AuxArgument : public Argument
    {
    public:
	typedef Argument Base;
	T_sp 		_Expression;
        AuxArgument() : Argument(_Nil<T_O>()), _Expression(_Nil<T_O>()) {};
	AuxArgument(Symbol_sp target,T_sp exp) : Argument(target), _Expression(exp) {};
        DECLARE_onHeapScanGCRoots();
	string asString() const;
    };





    
    // Don't derive from gc::GCObject so that these classes won't be treated as Lisp classes by GC system - thi
    // this will be a problem because some of the have smart_ptr's in them
    class DynamicScopeManager : gctools::StackBoundClass 
    {
    private:
	int	_beginTop;
	int	_endTop;
    public:
	virtual void new_binding(const Argument& argument, T_sp val);
	virtual bool lexicalElementBoundP(const Argument& argument) {N_A_();};
	void pushSpecialVariableAndSet(Symbol_sp sym, T_sp val);
	explicit DynamicScopeManager();
	explicit DynamicScopeManager(Symbol_sp sym,T_sp newVal);

	void dump() const;

	virtual T_sp lexenv() const;

	virtual ~DynamicScopeManager();
    };



    class ValueEnvironmentDynamicScopeManager : public DynamicScopeManager
    {
    private:
	ValueEnvironment_sp 	_Environment;
    public:
	ValueEnvironmentDynamicScopeManager(ValueEnvironment_sp env) : _Environment(env) {};
    public:
	/*! This is used for creating binds for lambda lists */
	virtual void new_binding(const Argument& argument, T_sp val);
	void new_variable(Cons_sp classifiedVariable, T_sp val);
	void new_special(Cons_sp classifiedVariable);
	virtual bool lexicalElementBoundP(const Argument& argument);
	virtual T_sp lexenv() const { return this->_Environment; };
    };




    class ActivationFrameDynamicScopeManager : public DynamicScopeManager
    {
    private:
	ActivationFrame_sp 	_Frame;
    public:
	ActivationFrameDynamicScopeManager(ActivationFrame_sp frame) : _Frame(frame) {};
    public:
	virtual void new_binding(const Argument& argument, T_sp val);
	virtual bool lexicalElementBoundP(const Argument& argument);
	T_sp activationFrame() const { return this->_Frame; };
	virtual T_sp lexenv() const;
    };


    class StackFrameDynamicScopeManager : public DynamicScopeManager
    {
    private:
        gctools::smart_ptr<STACK_FRAME>        frame;
    public:
	StackFrameDynamicScopeManager(gctools::smart_ptr<STACK_FRAME> f) : frame(f) {};
    public:
	virtual void new_binding(const Argument& argument, T_sp val);
	virtual bool lexicalElementBoundP(const Argument& argument);
	T_sp activationFrame() const;
	virtual T_sp lexenv() const;
    };


};

#endif
