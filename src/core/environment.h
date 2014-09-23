/*
    File: environment.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
       
       
#ifndef	Environment_H //[
#define Environment_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "symbol.fwd.h"
#include "holder.h"
//#i n c l u d e "stringSet.fwd.h"
#include "hashTable.fwd.h"
#include "environment.fwd.h"
#include "activationFrame.fwd.h"
#include "cons.h"

namespace core
{

    SMART(ObjectDictionary);
    SMART(Name);


    class Environment_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,CorePkg,Environment_O,"Environment");
    public:
    protected:
	uint		_EnvId;
    public:
	static T_sp clasp_currentVisibleEnvironment(T_sp env);
	static ActivationFrame_sp clasp_getActivationFrame(T_sp env);
	static int clasp_countFunctionContainerEnvironments(T_sp env);
	static bool clasp_findValue(T_sp env, Symbol_sp sym, int& depth, int& index, bool& special,T_sp& value);
	static bool clasp_findFunction(T_sp env, T_sp functionName, int& depth, int& index, Function_sp& func);
	static bool clasp_findTag(T_sp env, Symbol_sp sym, int& depth, int& index);
	static bool clasp_findSymbolMacro(T_sp env, Symbol_sp sym, int& depth, int& index, bool& shadowed, Function_sp& func);
	static bool clasp_findMacro(T_sp env, Symbol_sp sym, int& depth, int& index, Function_sp& func);
        static bool clasp_lexicalSpecialP(T_sp env, Symbol_sp sym);
        static T_sp clasp_lookupValue(T_sp env, int depth, int index );
        static Function_sp clasp_lookupFunction(T_sp env, int depth, int index );
        static T_sp clasp_lookupTagbodyId(T_sp env, int depth, int index );
    protected:
	static void clasp_environmentStackFill(T_sp env, int level, stringstream& sout);
	static Cons_sp clasp_gather_metadata(T_sp env, Symbol_sp key);
	static string clasp_summaryOfContents(T_sp env);
    public:
	uint environmentId() const { return this->_EnvId;};
	void setEnvironmentId(uint id) { this->_EnvId = id;};
	virtual bool lexicalEnvironmentP() const { return false;};
	virtual bool functionContainerEnvironmentP() const { return false;};
	virtual bool unwindProtectEnvironmentP() const { return false;};
	virtual bool catchEnvironmentP() const { return false;};
	
	virtual void setupParent(Environment_sp environ);
	virtual Environment_sp getParentEnvironment() const;
    public:

	virtual void setRuntimeEnvironment(T_sp renv);
	virtual T_sp runtimeEnvironment() const;

	/*! Return true if the symbol is declared special in the lexical environment */
	virtual bool lexicalSpecialP(Symbol_sp sym) const;


	/*! Associate a symbol in the current environment to some meta-data */
	virtual T_sp setf_metadata(Symbol_sp key, T_sp val) {SUBIMP();};

	/*! Gather a list of all metadata with the key ordered from outermost environment
	  to the innermost one */
	virtual Cons_sp gather_metadata(Symbol_sp key) const;

	/*! Push metadata into a Cons associated with the symbol */
	virtual Cons_sp push_metadata(Symbol_sp key, T_sp val) {SUBIMP();};

	/*! Lookup metadata - return two values
	  The first is the value found or nil and the second is t if a value is found or nil if not */
	virtual T_mv localMetadata(core::Symbol_sp key) const;

	/*! Lookup metadata in the linked list of environments return
	 MultipleValues(value,t/nil if found, environment) */
	virtual T_mv lookupMetadata(Symbol_sp key) const;

    public:
	/*! Return a summary of the contents of only this environment
	 */
	virtual string summaryOfContents() const;
    private:
	virtual void _environmentStackFill(int level, stringstream& sout);
    public:
	virtual ActivationFrame_sp getActivationFrame() const;
    public:
	// Indexed lookup of values
	/*! Classify the symbol as a special or lexical variable
	  If the variable is lexical return (list lexical-var _depth_ _index).
	  If the variable is lexically special return (list special-var _symbol_).
	  Otherwise return nil.  
	*/
	Cons_sp classifyValue(Symbol_sp sym) const;
	virtual T_sp _lookupValue(int depth, int index) const;
	virtual Function_sp _lookupFunction(int depth, int index) const;
        virtual T_sp _lookupTagbodyId(int depth, int index) const {SUBIMP();};
    public:
	string environmentStackAsString();

	/*! Search down the stack for the symbol
	 * If not found return end()
	 */
	virtual bool _findValue(Symbol_sp sym, int& depth, int& index, bool& special, T_sp& value) const;
	virtual bool findValue(Symbol_sp sym, int& depth, int& index, bool& special, T_sp& value) const;

	/*! Return the most recent RuntimeVisibleEnvironment */
	virtual Environment_sp currentVisibleEnvironment() const;


	/*! Search down the stack for the symbol
	 * If not found return end()
	 */
	virtual bool _findFunction(T_sp functionName, int& depth, int& index, Function_sp& value) const;
	virtual bool findFunction(T_sp functionName, int& depth, int& index, Function_sp& value) const;



	virtual bool _findMacro(Symbol_sp sym, int& depth, int& index, Function_sp& value) const;
	virtual bool findMacro(Symbol_sp sym, int& depth, int& index, Function_sp& value) const;


	virtual bool _findSymbolMacro(Symbol_sp sym, int& depth, int& index, bool& shadowed, Function_sp& value) const;
	virtual bool findSymbolMacro(Symbol_sp sym, int& depth, int& index, bool& shadowed, Function_sp& value) const;


	/*! If the symbol is not found return nil 
	 If it is lexical return `(lexical-var ,symbol ,depth . ,index)
	 If it is a dynamic variable return `(special-var . ,symbol)
	*/
	Cons_sp classifyLookup(Symbol_sp sym) const;

//	virtual T_mv variable_lookup(Symbol_sp sym) const;
//	virtual T_mv variable_lookup(const string& package,const string& symStr) const;

	virtual bool _updateValue(Symbol_sp sym, T_sp value);
//	virtual bool updateValueDontThrow(Symbol_sp sym, T_sp value);


    public: // extend the environment with forms
	/*! Lookup the Form, if it doesn't exist return nil */
//	virtual Function_sp function_lookup(T_sp functionName);

	/*! Classify function lookup
	  If the function is not found return nil
	  If the function is lexical return `(lexical-function ,symbol ,depth ,index)
	  If the function is not lexical return `(global-function . ,symbol )
	  The function name is either a symbol or a cons of the form (setf XXXX).
	*/
	virtual Cons_sp classifyFunctionLookup(T_sp functionName) const;

	/*! Return ('lexical-tag depth . index ) or ('dynamic-tag depth . index) */
	virtual Cons_sp classifyTag(Symbol_sp tag);


	/*! Lookup the SymbolMacro, if it doesn't exist return nil */
	Function_sp lookupSymbolMacro(Symbol_sp sym, bool& found) const;

	virtual string __repr__() const;


	/*! Lookup a tagbody tag in the lexical environment and return the environment
	  that defines it return nil if you don't find it*/
	virtual Environment_sp find_tagbody_tag_environment(Symbol_sp tag) const;


	/*! Lookup a tagbody tag in the lexical environment and return the environment
	  that defines it return nil if you don't find it*/
	virtual Environment_sp find_block_named_environment(Symbol_sp tag) const;


	/*! Lookup a tagbody tag in the lexical environment and return the environment
	  that defines it return nil if you don't find it*/
	virtual Environment_sp find_unwindable_environment() const;



	/*! Find the current function environment
	  that defines it return nil if you don't find it*/
	virtual Environment_sp find_current_code_environment() const;

	virtual bool recognizesBlockSymbol(Symbol_sp sym) const;
	virtual int getBlockSymbolFrame(Symbol_sp sym) const;

	virtual bool _findTag(Symbol_sp tag, int& depth, int& index) const;
	bool findTag(Symbol_sp tag, int& depth, int& index ) const;

	virtual int countFunctionContainerEnvironments() const;


	Environment_O() : Base(), _EnvId(0) {};
	virtual ~Environment_O() {};
    };
};
TRANSLATE(core::Environment_O);







namespace core
{
    class LexicalEnvironment_O : public Environment_O
    {
	LISP_BASE1(Environment_O);
	LISP_CLASS(core,CorePkg,LexicalEnvironment_O,"LexicalEnvironment");
    protected:
	//! Use setupParent to update this
    	Environment_sp	_ParentEnvironment;
	
	//! Compiler information
	HashTableEq_sp  _Metadata;
    public:
	void initialize();
    public:
	LexicalEnvironment_O();
	virtual ~LexicalEnvironment_O() {};


	virtual void setupParent(Environment_sp environ);
	Environment_sp getParentEnvironment() const;

	virtual string summaryOfContents() const;

	/*! Associate a symbol in the current environment to some meta-data */
	T_sp setf_metadata(Symbol_sp key, T_sp val);

	/*! Gather a Cons (one element per environment)
	  of all metadata with the key ordered from outermost environment
	  to the innermost one. If the metadata isn't present in an environment
	then nothing is put into the list for that environment*/
	virtual Cons_sp gather_metadata(Symbol_sp key) const;

	/*! Push metadata into a Cons associated with the symbol */
	Cons_sp push_metadata(Symbol_sp key, T_sp val);

	T_mv localMetadata(core::Symbol_sp key) const;

	/*! Lookup metadata in the linked list of environments return
	 MultipleValues(value,t/nil if found, environment) */
	T_mv lookupMetadata(Symbol_sp key) const;
    };
};
template<> struct gctools::GCInfo<core::LexicalEnvironment_O> {
    static bool const NeedsInitialization = true;
    static bool const NeedsFinalization = false;
    static bool const Moveable = true;
    static bool constexpr Atomic = false;
};

TRANSLATE(core::LexicalEnvironment_O);



namespace core
{
    class RuntimeVisibleEnvironment_O : public LexicalEnvironment_O
    {
	LISP_BASE1(LexicalEnvironment_O);
	LISP_CLASS(core,CorePkg,RuntimeVisibleEnvironment_O,"RuntimeVisibleEnvironment");
    protected:
        T_sp    _RuntimeEnvironment;
    public:
	void setRuntimeEnvironment(T_sp renv) { this->_RuntimeEnvironment = renv;};
	T_sp runtimeEnvironment() const { return this->_RuntimeEnvironment;};

	virtual bool _findValue(Symbol_sp sym, int& depth, int& index, bool& special, T_sp& value) const;
	virtual bool _findFunction(T_sp functionName, int& depth, int& index, Function_sp& value) const;
	virtual bool _findTag(Symbol_sp tag, int& depth, int& index) const;

	virtual Environment_sp currentVisibleEnvironment() const;

	RuntimeVisibleEnvironment_O();
	virtual ~RuntimeVisibleEnvironment_O() {};
    };
};
template<> struct gctools::GCInfo<core::RuntimeVisibleEnvironment_O> {
    static bool const NeedsInitialization = true;
    static bool const NeedsFinalization = false;
    static bool const Moveable = true;
    static bool constexpr Atomic = false;
};

TRANSLATE(core::RuntimeVisibleEnvironment_O);





namespace core
{

#if 0 // depreciated
    struct SavedSpecial
    {
	Symbol_sp 	_Symbol;
	T_sp 		_SavedValue;
    };
#endif

#if 0 // depreciated
    struct VariableDeclarations
    {
	Symbol_sp 	_ScopeKind; // :lexical :special
	Cons_sp 	_Declarations; // a-list see variable-information
    };
#endif
    class ValueEnvironment_O : public RuntimeVisibleEnvironment_O
    {
	LISP_BASE1(RuntimeVisibleEnvironment_O);
	LISP_CLASS(core,CorePkg,ValueEnvironment_O,"ValueEnvironment");
        void initialize();
    protected:
	/*! Maps symbols to their index within the activation frame or if the index is -1 then the symbol is locally special */
	HashTableEq_sp          _SymbolIndex;
	ActivationFrame_sp 	_ActivationFrame;
    public:
	static ValueEnvironment_sp createSingleTopLevelEnvironment();

	/*! Create an environment that extends a parent environment,
	 Pass a Cons of 2-element conses that contain either `(lexical ,symbol-name) or `(special ,symbol-name) 
	that distinguish if the symbol-name is a lexical one or a special one */
	static ValueEnvironment_sp createForLambdaListHandler(LambdaListHandler_sp llh, T_sp parent);
	/*! Create a fixed size environment for passing values to a function.
	 This is used to maintain runtime-environment information. */
	static ValueEnvironment_sp createForNumberOfEntries(int numberOfArguments, T_sp parent);

	static ValueEnvironment_sp createForLocallySpecialEntries(Cons_sp specials, T_sp parent);
    private:
	void setupForLambdaListHandler(LambdaListHandler_sp llh, Environment_sp parent);
    public:
	virtual T_sp _lookupValue(int depth, int index) const;
    public:
	/*! Return a summary of the contents of only this environment
	 */
	virtual string summaryOfContents() const;
#if 0
    private:
	void _environmentStackFill(int level, stringstream& sout);
    public:
	string environmentStackAsString();
#endif
	string allLocalNames() const;
	Cons_sp allLocalNamesAsCons() const;


	/*! Attach the lexical variable symbol to an index */
	void defineLexicalBinding(Symbol_sp sym, int idx);

	/*! define a special variable symbol */
	void defineSpecialBinding(Symbol_sp sym);

	/*! Search down the stack for the symbol
	 * If not found return false.
	 */
	bool _findValue(Symbol_sp sym, int& depth, int& level, bool& special, T_sp& value) const;

	/*! Lexical variable bindings shadow symbol macros so return false if the passed
	  symbol is a lexical variable. */
	bool _findSymbolMacro(Symbol_sp sym, int& depth, int& level, bool& shadowed, Function_sp& func) const;

	/*! Return true if the symbol is declared special in the lexical environment */
	bool lexicalSpecialP(Symbol_sp sym) const;


	bool _updateValue(Symbol_sp sym, T_sp value);


	/*! Extend the binder with the symbol/value pair and return the value */
	T_sp new_binding(Symbol_sp sym, int idx, T_sp value);

	ActivationFrame_sp getActivationFrame() const;

	bool activationFrameElementBoundP(int idx) const;
#if 0
	/*! If the symbol is global then look in the global stack
	 * Otherwise look up the local stack.  If the symbol isn't found then throw an exception
	 */
	T_sp oget(Symbol_sp sym);

	template <class o_class>
	gctools::smart_ptr<o_class> get(Symbol_sp sym)
	{
	    return safe_downcast<o_class>(this->oget(sym));
	}

#endif


	ValueEnvironment_O();
	virtual ~ValueEnvironment_O();
    };
};
template<> struct gctools::GCInfo<core::ValueEnvironment_O> {
    static bool const NeedsInitialization = true;
    static bool const NeedsFinalization = false;
    static bool const Moveable = true;
    static bool constexpr Atomic = false;
};

TRANSLATE(core::ValueEnvironment_O);




namespace core
{
    SMART(FunctionValueEnvironment);
    class FunctionValueEnvironment_O : public RuntimeVisibleEnvironment_O
    {
	LISP_BASE1(RuntimeVisibleEnvironment_O);
	LISP_CLASS(core,CorePkg,FunctionValueEnvironment_O,"FunctionValueEnvironment");
    public:
	void	initialize();
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    protected:
	/*! Map function names to Fixnum indices.
	  A function name is either a symbol or a cons of the form (setf XXXX) */
	HashTableEqual_sp 	_FunctionIndices;
	FunctionFrame_sp 	_FunctionFrame;
    public:
	/*! Create an environment that extends a parent environment
	 */
	static FunctionValueEnvironment_sp createEmpty(Environment_sp parent);
	static FunctionValueEnvironment_sp createForEntries(int numEntries, Environment_sp parent);
	ActivationFrame_sp getActivationFrame() const;
    public:
	virtual string summaryOfContents() const;

	/*! Extend the environment with the form bound to the symbol */
	int bind_function(T_sp functionName, Function_sp form);

	/*! Search down the stack for the symbol
	 * If not found return end()
	 */
	bool _findFunction(T_sp functionName, int& depth, int& level, Function_sp& func) const;

    public:


	DEFAULT_CTOR_DTOR(FunctionValueEnvironment_O);
    };
};
template<> struct gctools::GCInfo<core::FunctionValueEnvironment_O> {
    static bool const NeedsInitialization = true;
    static bool const NeedsFinalization = false;
    static bool const Moveable = true;
    static bool constexpr Atomic = false;
};

TRANSLATE(core::FunctionValueEnvironment_O);








namespace core
{

#if 0 // depreciated
    class HandlerHolder
    {
    private:
	Class_sp		_Condition;
	Function_sp	_Handler;
    public:
	HandlerHolder() {};
	void setup(Class_sp mc, Function_sp exec)
	{
	    this->_Condition = mc;
	    this->_Handler = exec;
	}
	Class_sp getCondition() { return this->_Condition;};
	Function_sp getHandler() { return this->_Handler;};
    };
#endif

    class CompileTimeEnvironment_O : public LexicalEnvironment_O
    {
	LISP_BASE1(LexicalEnvironment_O);
	LISP_CLASS(core,CorePkg,CompileTimeEnvironment_O,"CompileTimeEnvironment");
    public:

	virtual ActivationFrame_sp getActivationFrame() const;

	virtual Environment_sp currentVisibleEnvironment() const;

	virtual bool _findValue(Symbol_sp sym, int& depth, int& index, bool& special, T_sp& value) const;

	CompileTimeEnvironment_O();
	virtual ~CompileTimeEnvironment_O() {};
    };
};
TRANSLATE(core::CompileTimeEnvironment_O);












namespace core
{
    SMART(UnwindProtectEnvironment);
    class UnwindProtectEnvironment_O : public CompileTimeEnvironment_O
    {
	LISP_BASE1(CompileTimeEnvironment_O);
	LISP_CLASS(core,CorePkg,UnwindProtectEnvironment_O,"UnwindProtectEnvironment");
    public:
	void	initialize();
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    public:
	static UnwindProtectEnvironment_sp make( Environment_sp parent);
    public:
	virtual string summaryOfContents() const;
    public:
	DEFAULT_CTOR_DTOR(UnwindProtectEnvironment_O);


	/*! Lookup a tagbody tag in the lexical environment and return the environment
	  that defines it return nil if you don't find it*/
	virtual Environment_sp find_unwindable_environment() const;


    };
};
template<> struct gctools::GCInfo<core::UnwindProtectEnvironment_O> {
    static bool const NeedsInitialization = true;
    static bool const NeedsFinalization = false;
    static bool const Moveable = true;
    static bool constexpr Atomic = false;
};
TRANSLATE(core::UnwindProtectEnvironment_O);














namespace core
{
    SMART(BlockEnvironment);
    class BlockEnvironment_O : public CompileTimeEnvironment_O
    {
	LISP_BASE1(CompileTimeEnvironment_O);
	LISP_CLASS(core,CorePkg,BlockEnvironment_O,"BlockEnvironment");
    public:
	void	initialize();
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    private:
	Symbol_sp 	_BlockSymbol;
    public:
//	typedef vector<HandlerHolder>::iterator	handlerIterator;
    public:
	static BlockEnvironment_sp create(T_sp parent);
	static BlockEnvironment_sp make(Symbol_sp blockSymbol, T_sp parent);
    public:
	virtual string summaryOfContents() const;
    public:

	Symbol_sp getBlockSymbol() const { return this->_BlockSymbol;};
	void setBlockSymbol(Symbol_sp sym ) { this->_BlockSymbol = sym;};

	bool recognizesBlockSymbol(Symbol_sp sym) const;
//        int getBlockSymbolFrame(Symbol_sp sym) const;

	/*! Lookup a tagbody tag in the lexical environment and return the environment
	  that defines it return nil if you don't find it*/
	virtual Environment_sp find_block_named_environment(Symbol_sp tag) const;


	DEFAULT_CTOR_DTOR(BlockEnvironment_O);
    };
};
template<> struct gctools::GCInfo<core::BlockEnvironment_O> {
    static bool const NeedsInitialization = true;
    static bool const NeedsFinalization = false;
    static bool const Moveable = true;
    static bool constexpr Atomic = false;
};
TRANSLATE(core::BlockEnvironment_O);




namespace core
{
    SMART(CatchEnvironment);
    class CatchEnvironment_O : public CompileTimeEnvironment_O
    {
	LISP_BASE1(CompileTimeEnvironment_O);
	LISP_CLASS(core,CorePkg,CatchEnvironment_O,"CatchEnvironment");
    public:
	void	initialize();
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    public:
	static CatchEnvironment_sp make(Environment_sp parent);
    public:
	virtual string summaryOfContents() const;
    public:
	DEFAULT_CTOR_DTOR(CatchEnvironment_O);
    };
};
template<> struct gctools::GCInfo<core::CatchEnvironment_O> {
    static bool const NeedsInitialization = true;
    static bool const NeedsFinalization = false;
    static bool const Moveable = true;
    static bool constexpr Atomic = false;
};
TRANSLATE(core::CatchEnvironment_O);




namespace core
{

    SMART(FunctionContainerEnvironment);
    class FunctionContainerEnvironment_O : public CompileTimeEnvironment_O
    {
	LISP_BASE1(CompileTimeEnvironment_O);
	LISP_CLASS(core,CorePkg,FunctionContainerEnvironment_O,"FunctionContainerEnvironment");
    public:
	void	initialize();
    public:
	static FunctionContainerEnvironment_sp create(Environment_sp parent);
	static FunctionContainerEnvironment_sp make( Environment_sp parent);
    public:
	virtual string summaryOfContents() const;
    public:

	/*! Lookup a tagbody tag in the lexical environment and return the environment
	  that defines it return nil if you don't find it*/
	virtual Environment_sp find_current_code_environment() const;

	virtual int countFunctionContainerEnvironments() const;

	DEFAULT_CTOR_DTOR(FunctionContainerEnvironment_O);
    };
};
template<> struct gctools::GCInfo<core::FunctionContainerEnvironment_O> {
    static bool const NeedsInitialization = true;
    static bool const NeedsFinalization = false;
    static bool const Moveable = true;
    static bool constexpr Atomic = false;
};
TRANSLATE(core::FunctionContainerEnvironment_O);




namespace core
{

    FORWARD(TagbodyEnvironment);
    class TagbodyEnvironment_O : public RuntimeVisibleEnvironment_O
    {
	LISP_BASE1(RuntimeVisibleEnvironment_O);
	LISP_CLASS(core,CorePkg,TagbodyEnvironment_O,"TagbodyEnvironment");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(TagbodyEnvironment_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit TagbodyEnvironment_O(core::Class_sp const& mc) : T_O(mc), Environment(mc) {};
//    virtual ~TagbodyEnvironment_O() {};
    public:
	void initialize();
    private: // instance variables here
	HashTableEq_sp                  _Tags;
        gctools::Vec0<Cons_sp>	_TagCode;
	ActivationFrame_sp 	        _ActivationFrame;
    public: // Codes here
	static TagbodyEnvironment_sp make(Environment_sp env);
    public:

	virtual ActivationFrame_sp getActivationFrame() const;

	/*! Return the code that corresponds to the tag index */
	Cons_sp codePos(int index) const;

	/*! Return true if the tag is found and return the depth and index of the tag */
	virtual bool _findTag(Symbol_sp tag, int& depth, int& index) const;

	virtual string summaryOfContents() const;

	/*! Associate a tag(Symbol) with the position in the tagbody (tag|form)* list */
	int addTag(Symbol_sp tag, Cons_sp tagbodyPos);

	/*! Look for the tag in the environment */
	Cons_sp find(Symbol_sp tag) const;

	/*! Return all of the allowed tags as a string */
	string tagsAsString() const;


	/*! Lookup a tagbody tag in the lexical environment and return the environment
	  that defines it return nil if you don't find it*/
	virtual Environment_sp find_tagbody_tag_environment(Symbol_sp tag) const;

	
    }; // TagbodyEnvironment class
    
}; // core namespace
template<> struct gctools::GCInfo<core::TagbodyEnvironment_O> {
    static bool const NeedsInitialization = true;
    static bool const NeedsFinalization = false;
    static bool const Moveable = true;
    static bool constexpr Atomic = false;
};
TRANSLATE(core::TagbodyEnvironment_O);







namespace core
{

    FORWARD(MacroletEnvironment);
    class MacroletEnvironment_O : public CompileTimeEnvironment_O
    {
	LISP_BASE1(CompileTimeEnvironment_O);
	LISP_CLASS(core,CorePkg,MacroletEnvironment_O,"MacroletEnvironment");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(MacroletEnvironment_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit MacroletEnvironment_O(core::Class_sp const& mc) : T_O(mc), Environment(mc) {};
//    virtual ~MacroletEnvironment_O() {};
    public:
	void initialize();
    private: // instance variables here
        HashTableEq_sp          _Macros;
    public: // Codes here
	static MacroletEnvironment_sp make(Environment_sp env);
    public:
	void addMacro(Symbol_sp name, Function_sp macro);


	bool _findMacro(Symbol_sp sym, int& depth, int& level, Function_sp& func) const;

	virtual string summaryOfContents() const;

	
    }; // MacroletEnvironment class
    
}; // core namespace
template<> struct gctools::GCInfo<core::MacroletEnvironment_O> {
    static bool const NeedsInitialization = true;
    static bool const NeedsFinalization = false;
    static bool const Moveable = true;
    static bool constexpr Atomic = false;
};
TRANSLATE(core::MacroletEnvironment_O);






namespace core
{

    FORWARD(SymbolMacroletEnvironment);
    class SymbolMacroletEnvironment_O : public CompileTimeEnvironment_O
    {
	LISP_BASE1(CompileTimeEnvironment_O);
	LISP_CLASS(core,CorePkg,SymbolMacroletEnvironment_O,"SymbolMacroletEnvironment");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(SymbolMacroletEnvironment_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit SymbolMacroletEnvironment_O(core::Class_sp const& mc) : T_O(mc), Environment(mc) {};
//    virtual ~SymbolMacroletEnvironment_O() {};
    public:
	void initialize();
    private: // instance variables here
	HashTableEq_sp          _Macros;
    public: // Codes here
	static SymbolMacroletEnvironment_sp make(Environment_sp env);
    public:
	
	void addSymbolMacro(Symbol_sp sym, Function_sp expansion);

	bool _findSymbolMacro(Symbol_sp sym, int& depth, int& level, bool& shadowed, Function_sp& func) const;

	void throwErrorIfSymbolMacrosDeclaredSpecial(Cons_sp specialDeclaredSymbols) const;

	virtual string summaryOfContents() const;


    }; // SymbolMacroletEnvironment class
    
}; // core namespace
template<> struct gctools::GCInfo<core::SymbolMacroletEnvironment_O> {
    static bool const NeedsInitialization = true;
    static bool const NeedsFinalization = false;
    static bool const Moveable = true;
    static bool constexpr Atomic = false;
};

TRANSLATE(core::SymbolMacroletEnvironment_O);













namespace core
{
    // A simple environment that maps symbols to objects to allow me to 
    // call old style make_init functions
    class GlueEnvironment_O : public Environment_O
    {
	LISP_BASE1(Environment_O);
	LISP_CLASS(core,CorePkg,GlueEnvironment_O,"GlueEnvironment");
        void initialize();
    protected:
	/*! Maps symbols to their index within the activation frame or if the index is -1 then the symbol is locally special */
	HashTableEq_sp          _Map;
	Cons_sp 		_Args;
    public:
	/*! Create an environment that extends a parent environment,
	 Pass a Cons of 2-element conses that contain either `(lexical ,symbol-name) or `(special ,symbol-name) 
	that distinguish if the symbol-name is a lexical one or a special one */
	static GlueEnvironment_sp create(Cons_sp parts);

	/*! Return the arguments as a list */
	Cons_sp args() const { return this->_Args;};

//	T_mv variable_lookup(Symbol_sp val) const;

	GlueEnvironment_O() : Base() {};
	virtual ~GlueEnvironment_O() {};
    };
};


TRANSLATE(core::GlueEnvironment_O);



namespace core {
    T_sp af_environmentActivationFrame(T_sp env);

    bool af_updateValue(T_sp env, Symbol_sp sym, T_sp val);

    T_mv core_lexicalFunction(T_sp sym, T_sp env);
    T_mv core_lexicalMacroFunction(T_sp sym, T_sp env);
};


#endif //]
