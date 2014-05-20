       
       
#ifndef	llvmo_Environment_H //[
#define llvmo_Environment_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "core/foundation.h"
#include "core/object.h"
#include "core/symbol.h"
#include "core/holder.h"
#include "core/stringSet.fwd.h"
#include "core/serialize.fwd.h"
#include "core/activationFrame.fwd.h"
#include "core/holder.h"
#include "core/cons.h"
#include "llvmoPackage.h"
#include "environment.fwd.h"

namespace llvmo
{



    class Environment_O : public core::T_O
    {
	LISP_BASE1(core::T_O);
	LISP_CLASS(LlvmoPkg,Environment_O,"Environment");
    public:
    protected:
	//! Use setParent to update this
    	Environment_sp	_ParentEnvironment;
	uint		_Id;
	uint		_Depth;
	core::SymbolMap<core::T_O>	_MetaData;
    public:
	void initialize();
    public:
	uint getId() const { return this->_Id;};
	uint getDepth() const { return this->_Depth;};
	void setParent(Environment_sp environ);
	Environment_sp getParentEnvironment() const;
    public:
	/*! Return a summary of the contents of only this environment
	 */
	virtual string summaryOfContents() const;
    private:
	void _environmentStackFill(int level, stringstream& sout);
    public:
	virtual core::ActivationFrame_sp activationFrame() const { if (this->isNil()) return core::ActivationFrame_O::_nil; SUBIMP();};
    public:
	// Indexed lookup of values
	core::Cons_sp classifyValue(core::Symbol_sp sym) const;
//	virtual core::T_sp lookupValue(int depth, int index) const;
    public:
	string environmentStackAsString();

	core::T_sp setf_metaData(core::Symbol_sp key, core::T_sp val) {this->_MetaData.set(key,val); return val;};
	core::Cons_sp push_metaData(core::Symbol_sp key, core::T_sp val) {
	    core::Cons_sp one = core::Cons_O::create(val,this->metaData(key),_lisp);
	    this->_MetaData.set(key,one);
	    return one;
	}

	core::T_sp metaData(core::Symbol_sp key);

	bool hasMetaData(core::Symbol_sp key) { return this->_MetaData.contains(key);};
	

	/*! Search down the stack for the symbol
	 * If not found return end()
	 */
	virtual bool _findValue(core::Symbol_sp sym, int& depth, int& index ) const;
	virtual bool findValue(core::Symbol_sp sym, int& depth, int& index ) const;


	/*! Search down the stack for the symbol
	 * If not found return end()
	 */
	virtual bool _findFunction(core::Symbol_sp sym, int& depth, int& index) const;
	virtual bool findFunction(core::Symbol_sp sym, int& depth, int& index) const;

	/*! If the symbol is not found return nil 
	 If it is lexical return `(lexical-var ,symbol ,depth . ,index)
	 If it is a dynamic variable return `(special-var . ,symbol)
	*/
	core::Cons_sp classifyLookup(core::Symbol_sp sym) const;

//	virtual core::T_sp lookup(core::Symbol_sp sym) const;
//	virtual core::T_sp lookup(const string& package,const string& symStr) const;

//	virtual void updateValue(core::Symbol_sp sym, core::T_sp value);
//	virtual bool updateValueDontThrow(core::Symbol_sp sym, core::T_sp value);


    public: // extend the environment with forms
	/*! Lookup the Form, if it doesn't exist return nil */
//	virtual core::Function_sp lookup_function(core::Symbol_sp sym);

	/*! Classify function lookup
	  If the function is not found return nil
	  If the function is lexical return `(lexical-function ,symbol ,depth . ,index)
	  If the function is not lexical return `(special-function . ,symbol )
	*/
	virtual core::Cons_sp classifyFunctionLookup(core::Symbol_sp sym) const;
	


	/*! Lookup the SymbolMacro, if it doesn't exist return nil */
	core::Function_sp lookupSymbolMacro(core::Symbol_sp sym, bool& found) const;

	virtual string __repr__() const;


	/*! Lookup a tagbody tag in the lexical environment and return the environment
	  that defines it return nil if you don't find it*/
	virtual Environment_sp find_tagbody_tag_environment(core::Symbol_sp tag) const;


	/*! Lookup a tagbody tag in the lexical environment and return the environment
	  that defines it return nil if you don't find it*/
	virtual Environment_sp find_block_named_environment(core::Symbol_sp tag) const;


	/*! Dump the environment and its parents to the screen - call from gdb*/
	void dump();

	Environment_O(const core::MetaClass_sp& mc) : Base(mc) {};
	virtual ~Environment_O() {};
    };













    struct SavedSpecial
    {
	core::Symbol_sp 	_Symbol;
	core::T_sp 		_SavedValue;
    };


    class ValueEnvironment_O : public Environment_O
    {
	LISP_BASE1(Environment_O);
	LISP_CLASS(LlvmoPkg,ValueEnvironment_O,"ValueEnvironment");
    public:
	typedef	map<core::Symbol_sp,int>::iterator		symbol_iterator;
	typedef	map<core::Symbol_sp,int>::const_iterator	const_symbol_iterator;
    protected:
	/*! Maps symbols to their index within the activation frame or if the index is -1 then the symbol is locally special */
	map<core::Symbol_sp,int>			_SymbolIndex;
    public:
	static ValueEnvironment_sp createSingleTopLevelEnvironment();

	/*! Create an environment that extends a parent environment,
	 Pass a Cons of 2-element conses that contain either `(lexical ,symbol-name) or `(special ,symbol-name) 
	that distinguish if the symbol-name is a lexical one or a special one */
	static ValueEnvironment_sp createForLambdaListHandler(core::LambdaListHandler_sp llh, Environment_sp parent);
	/*! Create a fixed size environment for passing values to a function.
	 This is used to maintain runtime-environment information. */
	static ValueEnvironment_sp createForArgumentPassing(int numberOfArguments, Environment_sp parent);

    private:
	void setupForLambdaListHandler(core::LambdaListHandler_sp llh, Environment_sp parent);
    public:
//	virtual core::T_sp lookupValue(int depth, int index) const;
    public:
	/*! Return a summary of the contents of only this environment
	 */
	virtual string summaryOfContents() const;
    private:
	void _environmentStackFill(int level, stringstream& sout);
    public:
	string environmentStackAsString();

	string allLocalNames() const;
	core::Cons_sp allLocalNamesAsCons() const;

	/*! Search down the stack for the symbol
	 * If not found return end()
	 */
	bool _findValue(core::Symbol_sp sym, int& depth, int& level ) const;

	/*! Return the depth/index of the symbol as a Cons or nil if it isn't a lexical variable */
	core::Cons_sp lexical_variable_p(core::Symbol_sp sym) const;

//	void updateValue(core::Symbol_sp sym, core::T_sp value);


	/*! Extend the binder with the symbol/value pair and return the value */
	core::T_sp new_binding(core::Symbol_sp sym, int idx, core::T_sp value);

#if 0
	/*! If the symbol is global then look in the global stack
	 * Otherwise look up the local stack.  If the symbol isn't found then throw an exception
	 */
	core::T_sp oget(core::Symbol_sp sym);

	template <class o_class>
	    boost::shared_ptr<o_class> get(core::Symbol_sp sym)
	{
	    return safe_downcast<o_class>(this->oget(sym));
	}

#endif


	/*! Dump the environment and its parents to the screen - call from gdb*/
	void dump();

	ValueEnvironment_O(const core::MetaClass_sp& mc);
	virtual ~ValueEnvironment_O();
    };







    class HandlerHolder
    {
    private:
	core::MetaClass_sp		_Condition;
	core::Function_sp	_Handler;
    public:
	HandlerHolder() {};
	void setup(core::MetaClass_sp mc, core::Function_sp exec)
	{
	    this->_Condition = mc;
	    this->_Handler = exec;
	}
	core::MetaClass_sp getCondition() { return this->_Condition;};
	core::Function_sp getHandler() { return this->_Handler;};
    };




    



    SMART(BlockEnvironment);
    class BlockEnvironment_O : public Environment_O
    {
	LISP_BASE1(Environment_O);
	LISP_CLASS(LlvmoPkg,BlockEnvironment_O,"BlockEnvironment");
    public:
	void	initialize();
	void	archiveBase(core::ArchiveP node);
    private:
	core::Symbol_sp 	_BlockSymbol;
    public:
	typedef vector<HandlerHolder>::iterator	handlerIterator;
    public:
	static BlockEnvironment_sp create(Environment_sp parent);
	static BlockEnvironment_sp create(core::Symbol_sp blockSymbol, Environment_sp parent);
    public:
	virtual string summaryOfContents() const;
    public:

	core::Symbol_sp getBlockSymbol() const { return this->_BlockSymbol;};
	void setBlockSymbol(core::Symbol_sp sym) { this->_BlockSymbol = sym;};


	/*! Lookup a tagbody tag in the lexical environment and return the environment
	  that defines it return nil if you don't find it*/
	virtual Environment_sp find_block_named_environment(core::Symbol_sp tag) const;


	DEFAULT_CTOR_DTOR(BlockEnvironment_O);
    };



    SMART(FunctionEnvironment);
    class FunctionEnvironment_O : public Environment_O
    {
	LISP_BASE1(Environment_O);
	LISP_CLASS(LlvmoPkg,FunctionEnvironment_O,"FunctionEnvironment");
    public:
	void	initialize();
	void	archiveBase(core::ArchiveP node);
    protected:
	map<core::Symbol_sp,int>	_FunctionIndices;
    public:
	/*! Create an environment that extends a parent environment
	 */
	static FunctionEnvironment_sp create(Environment_sp parent);
	static FunctionEnvironment_sp create(int numEntries, Environment_sp parent);
    public:
	virtual string summaryOfContents() const;

	/*! Extend the environment with the form bound to the symbol */
	void bind_function(core::Symbol_sp sym, core::Function_sp form);

	/*! Search down the stack for the symbol
	 * If not found return end()
	 */
	bool _findFunction(core::Symbol_sp sym, int& depth, int& level ) const;

    public:


	DEFAULT_CTOR_DTOR(FunctionEnvironment_O);
    };


}; // namespace llvmo


namespace llvmo
{
    FORWARD(TagbodyEnvironment);
    class TagbodyEnvironment_O : public Environment_O
    {
	LISP_BASE1(Environment_O);
	LISP_CLASS(LlvmoPkg,TagbodyEnvironment_O,"TagbodyEnvironment");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(TagbodyEnvironment_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit TagbodyEnvironment_O(core::MetaClass_sp const& mc) : T_O(mc), Environment(mc) {};
//    virtual ~TagbodyEnvironment_O() {};
    public:
	void initialize();
    public:
	typedef core::SymbolMap<core::Cons_O>::iterator		iterator;
	typedef core::SymbolMap<core::Cons_O>::const_iterator	const_iterator;
    private: // instance variables here
	core::SymbolMap<core::Cons_O>	_Tags;
    public: // Functions here
	static TagbodyEnvironment_sp create(Environment_sp env);
    public:
	const_iterator begin() const { return this->_Tags.begin(); };
	const_iterator end() const { return this->_Tags.end(); };
	iterator begin() { return this->_Tags.begin(); };
	iterator end() { return this->_Tags.end(); };

	/*! Associate a tag(Symbol) with the position in the tagbody (tag|form)* list */
	void addTag(core::Symbol_sp tag, core::Cons_sp tagbodyPos);

	/*! Look for the tag in the environment */
	const_iterator find(core::Symbol_sp tag) const;

	/*! Return all of the allowed tags as a string */
	string tagsAsString() const;


	/*! Lookup a tagbody tag in the lexical environment and return the environment
	  that defines it return nil if you don't find it*/
	virtual Environment_sp find_tagbody_tag_environment(core::Symbol_sp tag) const;

	
    }; // TagbodyEnvironment class
    
}; // core namespace






TRANSLATE(llvmo::Environment_O);
TRANSLATE(llvmo::ValueEnvironment_O);
TRANSLATE(llvmo::FunctionEnvironment_O);
TRANSLATE(llvmo::BlockEnvironment_O);
TRANSLATE(llvmo::TagbodyEnvironment_O);









#endif //]


