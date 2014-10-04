/*
    File: lambdaListHandler.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister

CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

See directory 'clasp/licenses' for full details.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
#ifndef	LambdaListHandler_H //[
#define LambdaListHandler_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "symbolSet.h"
#include "vectorObjects.fwd.h"
#include "ql.h"
#include "arguments.h"
#include "symbol.h"

namespace core {

    typedef enum { required, optional, dot_rest, rest, keyword, allowOtherKeys, aux } ArgumentMode;

    /*! A LambdaListHandler converts lambda-lists of all different types into
      an object that binds arguments to symbols in an environment.
      There is a subclass for every possible type of lambda-list namely:
      ordinary
      generic-function
      specialized
      macro
      destructuring
      boa
      defsetf
      deftype
      define-modify-macro
      define-method-combination
      See CLHS 3.4
    */

    template <class ArgumentType>
    string asString(gctools::Vec0<ArgumentType> const& vec)
    {
	stringstream ss;
	for (typename gctools::Vec0<ArgumentType>::const_iterator it=vec.begin(); it!=vec.end(); it++ )
	{
	    ss << " " << it->asString();
	}
	return ss.str();
    }
}; // core


namespace core
{



    struct TargetClassifier
    {
	SymbolSet_sp 	_SpecialSymbols;
	/*! symbols that are seen in the lambda-list and are special are accumulated here */
	SymbolSet_sp    _LambdaListSpecials;
	int		lexicalIndex;
	ql::list	_AccumulatedClassifiedSymbols;
        std::set<int>   skipLexicalIndices;
	explicit TargetClassifier(const std::set<int>& skipLexicalIndices);
	explicit TargetClassifier(SymbolSet_sp specialSymbols, const std::set<int>& skipLexicalIndices);
	void classifyTarget(Argument& target);
	Cons_sp finalClassifiedSymbols();
	void targetIsSubLambdaList(Argument& target,LambdaListHandler_sp subHandler);
	int totalLexicalVariables() const { return this->lexicalIndex;};
        void advanceLexicalIndex();
    };




    class LambdaListHandler_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,CorePkg,LambdaListHandler_O,"LambdaListHandler");
    public: // virtual functions inherited from Object
	void	initialize();
#if defined(XML_ARCHIVE)
//	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
//	string	__repr__() const;
    protected: // instance variables
	/*! If _CreatesBindings is true then no bindings will be set by this
	  LambdaListHandler */
	bool				_CreatesBindings;
	Cons_sp 			_LambdaList;
	Cons_sp 			_ClassifiedSymbolList;
	SymbolSet_sp 			_SpecialSymbolSet;
	Cons_sp 			_DeclareSpecifierList;
        gctools::Vec0<RequiredArgument>	_RequiredArguments;
        gctools::Vec0<OptionalArgument>	_OptionalArguments;
	RestArgument			_RestArgument;
        T_sp                            _KeyFlag;
        gctools::Vec0<KeywordArgument>	_KeywordArguments;
	T_sp				_AllowOtherKeys;
        gctools::Vec0<AuxArgument>	_AuxArguments;
        gctools::gcstring	        _Comment;
	// -- calculated info --
	int				_NumberOfLexicalVariables;
	VectorObjects_sp 		_LexicalVariableNamesForDebugging;
    public:
	typedef gctools::Vec0<RequiredArgument>::iterator	requiredArgumentIterator;
	typedef gctools::Vec0<RequiredArgument>::const_iterator	const_requiredArgumentIterator;
	requiredArgumentIterator beginRequiredArguments() { return this->_RequiredArguments.begin(); };
	requiredArgumentIterator endRequiredArguments() { return this->_RequiredArguments.end();};
	const_requiredArgumentIterator beginRequiredArguments() const { return this->_RequiredArguments.begin(); };
	const_requiredArgumentIterator endRequiredArguments() const { return this->_RequiredArguments.end();};
    public:
	/*! The context can be 'ORDINARY, 'MACRO and other values - see throw_if_invalid_context */
	static LambdaListHandler_sp makeLambdaListHandler(Cons_sp lambda_list, Cons_sp declares, T_sp context);

	static Cons_mv process_single_dispatch_lambda_list(Cons_sp lambda_list, bool allow_first_argument_default_dispatcher = false);
	static Cons_sp process_macro_lambda_list(Cons_sp lambda_list );
    public:
	/*! Compile the argumentsInString into an argument handler and put the symbols into the
	  given package */
	static LambdaListHandler_sp createRecursive(Cons_sp lambda_list, Cons_sp declares, T_sp context, TargetClassifier& classifier);

	/*! Compile the lambda-list into an LambdaListHandler */
	static LambdaListHandler_sp create(Cons_sp lambda_list, Cons_sp declares, T_sp context, const std::set<int>& pureOutValues = std::set<int>() );

	/*! Create a simple LambdaListHandler that takes a fixed number of required arguments */
	static LambdaListHandler_sp create(int numArgs, const std::set<int>& pureOutValues = std::set<int>() );
    public:	// set up argument handling by hand

	/*! Use the declares to adjust the behavior of the LambdaListHandler.
	  (declare (single-dispatch-on {target-symbol})) - sets {target-symbol} as the argument to single dispatch on
	*/

	void setComment(const string& s) { this->_Comment = s;};
	string getComment() const { return this->_Comment.asStdString();};

	void process_declares(Cons_sp declares) {_OF(); SUBCLASS_MUST_IMPLEMENT();};

    public:
	static Cons_sp classifyOneSymbol(Symbol_sp sym);
	static Cons_sp classifySymbols(Cons_sp symbols);

    protected:
	static SymbolSet_sp identifySpecialSymbols(Cons_sp declares);
	void classifyTarget(Argument& target, int& lexicalIndex, const SymbolSet_sp& specialSymbols );
	void recursively_build_handlers_count_arguments(Cons_sp declares, T_sp context, TargetClassifier& classifier );
    public:
//	void process_declares(Cons_sp declares);
	void create_required_arguments(int numArgs, const std::set<int>& skipIndices );
    public:

	/*! Promote the argument with the given symbol to have a FrameIndex of 0 and all
	  arguments that formerly preceeded it will be slid up by one.
	  This is used to enable single dispatching on any required argument.
	  An exception will be thrown if the (target) symbol is not a valid target of the LambdaListHandler
	  (target) must be one of the required targets.
	Return the index of the target symbol in the LambdaListHandler */
	int single_dispatch_on_argument(Symbol_sp target);


	/*! Parse a lambda-list and a list of declares that apply to the lambda-list
	  context can be one of 'FUNCTION, 'MACRO and other values */
	virtual void parse_lambda_list_declares(Cons_sp lambda_list, Cons_sp declares, T_sp context, TargetClassifier& classifier);

	void createBindingsInScope_af(ActivationFrame_sp args,
				   DynamicScopeManager& scope );
	void createBindingsInScope_argArray(int n_args, ArgArray argArray,
				   DynamicScopeManager& scope );

	void createBindingsInScope_argArray_TPtr(int n_args, T_O* argArray[],
                                                 DynamicScopeManager& scope );

	/*! Return a list of expressions that can be evaluated in (env) to generate a list of values that would
	  be put into the classifiedSymbols */
	Cons_sp lambdaListParts(T_sp env);

	// ---------
	// Following are the methods that deal with preparing Lexical ActivationFrames for arguments

	/*! Return true if the LambdaListHandler only has required arguments */
	bool requiredLexicalArgumentsOnlyP() const;

	int numberOfRequiredArguments() const { return this->_RequiredArguments.size(); };
        int numberOfOptionalArguments() const { return this->_OptionalArguments.size(); };
        int numberOfRestArguments() const { return this->_RestArgument._ArgTarget.nilp() ? 0 : 1; };
        bool hasKeyFlag() const { return this->_KeyFlag.isTrue(); };
        int numberOfKeyArguments() const { return this->_KeywordArguments.size(); };
        int numberOfAuxArguments() const { return this->_AuxArguments.size(); };
        bool allowOtherKeys() const { return this->_AllowOtherKeys.notnilp(); };

//	uint _numberOfRequiredArguments() const;

	/*! The total number of arguments that will be bound by this handler in a lexical ActivationFrame */
	int numberOfLexicalVariables() const { return this->_NumberOfLexicalVariables;};


	/*! Return all of the symbols that this LambdaListHandler will fill classified as to whether they are special-var or lexical-var's */
	Cons_sp classifiedSymbols() const { return this->_ClassifiedSymbolList;};


	/*! Return a Cons of all lexical variable names extracted from this->_ClassifiedSymbolList
	  in the order that they appear in _ClassifiedSymbolList - this is used for
	  debugging - you can attach this symbols list (as a Vector of symbols) to runtime ActivationFrames */
	Cons_sp namesOfLexicalVariables() const;

	/*! Return a VectorObjects of the symbols of all of the lexical variables for attaching to ActivationFrames */
	VectorObjects_sp namesOfLexicalVariablesForDebugging();
    private:
	void calculateNamesOfLexicalVariablesForDebugging();
    public:

	/*! Return the processed lambda list as a MultipleValue object
	  Return these parts in order:
	  reqs opts rest key-flag keys allow-other-keys auxs
	  It works like:
	  * (si::process-lambda-list lambda-list context)
	  *
	  * Parses different types of lambda lists. CONTEXT may be MACRO,
	  * FTYPE, FUNCTION, METHOD or DESTRUCTURING-BIND, and determines the
	  * valid sytax. The output is made of several values:
	  *
	  * VALUES(0) = (N target-req1 ... )			; required values
	  * VALUES(1) = (N target-opt1 init1 target-flag1 ... )	; optional values
	  * VALUES(2) = target-rest-var				; rest-variable, if any
	  * VALUES(3) = key-flag				; T if &key was supplied
	  * VALUES(4) = (N key1 target-var1 init1 target-flag1 ... )	; keyword arguments
	  * VALUES(5) = allow-other-keys			; flag &allow-other-keys
	  * VALUES(6) = (N target-aux1 init1 ... )		; auxiliary variables
	  *
	  * 1°) The prefix "N" is an integer value denoting the number of
	  * variables which are declared within this section of the lambda
	  * list.
	  *
	  * 2°) The INIT* arguments are lisp forms which are evaluated when
	  * no value is provided.
	  *
	  * 3°) The FLAG* arguments is the name of a variable which holds a
	  * boolean value in case an optional or keyword argument was
	  * provided. If it is NIL, no such variable exists.
	  *
	  * 4) The target- values are conses that have the following structure
	  *      (special-var . _symbol_) where _symbol_ is the special var where to put the value
	  *      (lexical-var _symbol_ . _activationFrameIndex_) where _activationFrameIndex_ is the index where
	  *                        to write the value in the dest-activation-frame
	  */
	Cons_mv processLambdaListHandler() const;

	string __repr__() const;
	string partsAsString() const;

	LambdaListHandler_O();
	virtual ~LambdaListHandler_O();
    };
};
TRANSLATE(core::LambdaListHandler_O);
template<> struct gctools::GCInfo<core::LambdaListHandler_O> {
    static bool constexpr NeedsInitialization = true;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};





#endif //]
