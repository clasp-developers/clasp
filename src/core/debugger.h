#ifndef debugger_H
#define debugger_H


#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "stacks.h"
#include "conditions.h"

namespace core
{
/*! This class controls the single-step state of the Lisp interpreter
  in an exception safe way.
  When you want to force a form to execute in single step mode
  you declare a LispDebugger(lisp,true) in the scope where you will
  evaluate the form and then when the form finishes it will restore
  the single step state to what it was.
*/

    class LispDebugger 
    {
    private:
	bool			_CanContinue;
	T_sp			_Condition;
    public:
	/* Immediatly returns if we are not single stepping or if the step stack level
	   is less than the current stack level.
	   Otherwise print the next instruction to be evaluated and wait for
	   the user to indicate what they want to do. */
	static void step();
    public:

	/*! Print the current expression */
	void printExpression();

	/*! Invoke the debugger,
	  If the user is allowed to resume and opts to resume then return the resume object 
	*/
	T_sp invoke();

	InvocationHistoryFrame& currentFrame() const;

	LispDebugger(T_sp condition);
	LispDebugger();

	virtual ~LispDebugger()
	{_G();
	    _lisp->decrementDebuggerLevel();
	};
    };


    void af_backtrace();
    
    void initialize_debugging();


    extern "C" {
	void af_gotoIhsTop();
	void af_gotoIhsNext();
	void af_gotoIhsPrev();
	void af_printCurrentIhsFrame();
	void af_evalPrint(const string& expr);
    };
    
};
#endif
