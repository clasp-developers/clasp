
#ifndef	Conditions_H //[
#define Conditions_H


#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "lispStream.fwd.h"
#include "conditions.fwd.h"
#include "object.h"






namespace core
{
    class CandoException_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,CorePkg,CandoException_O,"CandoException");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(CandoException_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit CandoException_O(core::MetaClass_sp const& mc) : T_O(mc), T(mc) {};
//    virtual ~CandoException_O() {};
    public:
	static CandoException_sp create(const string& msg);
	static CandoException_sp create(const boost::format& fmt);

    private: // instance variables here
	string _message;
	
    public: // Functions here
	void setMessage(const string& msg) { this->_message = msg;};
	string message() const { return this->_message;};

    }; // CandoException class
    
}; // core namespace
TRANSLATE(core::CandoException_O);




namespace core 
{





    class Condition
    {
    private:
	T_sp	_ConditionObject;
    public:
	T_sp	conditionObject() const;
	void setConditionObject(T_sp co);
	string message() const;

	Condition(T_sp cond);
	Condition(const Condition& c);
	virtual ~Condition() throw ();
    };




};





namespace core
{

    T_sp af_makeCondition(T_sp datum, Cons_sp initializers);

    string af_conditionMessage(T_sp condition);

    void initialize_conditions();
}








#endif //]
