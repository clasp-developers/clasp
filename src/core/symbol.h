       
       
#ifndef	Symbol_H //[
#define Symbol_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "cons.h"
#include "activationFrame.h"
#include "environment.fwd.h"
#include "lisp.h"



#define KW(s) (_lisp->internKeyword(s))

namespace core {


    SMART(Package);
    SMART(Function);

    FORWARD(Symbol);
    class Symbol_O : public T_O
    {
        struct metadata_bootstrap_class {};
        struct metadata_gc_do_not_move {};
    public:
	Str_sp			_Name;
	Package_sp		_HomePackage;
	T_sp 			_Value;
	Function_sp 		_Function;
	bool			_IsSpecial;
	bool			_IsConstant;
	bool			_ReadOnlyFunction;
	Cons_sp			_PropertyList;
    private:
	friend class Class_O;
	friend class Package_O;
	friend class CoreExposer;
	LISP_BASE1(T_O);
	LISP_CLASS(core,ClPkg,Symbol_O,"Symbol");
    public:
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
#if defined(OLD_SERIALIZE)
	void	serialize(serialize::SNode node);
#endif
    public:
	/*! Create a Symbol that doesn't have a package or Metaclass defined */
//	static Symbol_sp create_classless_packageless(string const& name);

	static Symbol_sp create(const string& nm);
    public:
	string formattedName(bool prefixAlways) const;
    public:

	T_sp apply();
	T_sp funcall();

	// Include the apply function for symbols up to some high arity
//#include "symbol_apply_inc.h"

	void sxhash(HashGenerator& hg) const;

	bool isKeywordSymbol();
	Symbol_sp asKeywordSymbol();

	bool amp_symbol_p() const;


	/*! Return a pointer to the value cell */
	inline T_sp* valueReference() { return &(this->_Value);};


	void setReadOnly(bool b) { this->_IsConstant = true;};
	bool getReadOnly() const { return this->_IsConstant;};

	void setReadOnlyFunction(bool b) {this->_ReadOnlyFunction = true;};
	bool getReadOnlyFunction() const {return this->_ReadOnlyFunction;};

	/*! Return true if the symbol is dynamic/special */
	bool specialP() const { return this->_IsSpecial;};

	Symbol_sp copy_symbol(T_sp copy_properties) const;
	bool isExported();

	/*! Return the value slot of the symbol - throws if unbound */
	T_sp symbolValue() const;

	/*! Return the value slot of the symbol or UNBOUND if unbound */
	T_sp symbolValueUnsafe() const;

	/*! Return the dynamic value as type T */
	template <class T>
	gctools::smart_ptr<T> dyn() { return this->symbolValue().as<T>(); };

	void makeSpecial();

	void makeConstant(T_sp val);

	bool boundP() const;

	void makunbound();

	T_sp defparameter(T_sp obj);
	T_sp defconstant(T_sp obj);

	T_sp setf_symbolValue(T_sp obj);

	void setf_symbolValueReadOnlyOverRide(T_sp obj);

	bool isConstant() const { return this->_IsConstant;};

	/*! Set the global fnction value of this symbol */
	void setf_symbolFunction(Function_sp exec);

	/*! Return the global bound function */
	Function_sp symbolFunction();


	/*! Return true if the symbol has a function bound*/
	bool fboundp() const;

	const char* permanentName() const;

	string symbolNameAsString() const;

	Str_sp symbolName() const { return this->_Name;};
	Str_sp identifierName() const { return this->symbolName();};


	Package_sp getPackage() const;
	Package_sp homePackage() const { return this->getPackage();};
	void setPackage(Package_sp p);

	/*! Return the name of the symbol with the package prefix
	 * unless this symbol is in the current package
	 */
	string currentName() const;

	/*! Return the name of the symbol with the package prefix
	 * always attached
	 */
	string fullName() const;

	/*! Convenience function, export yourself and return yourself */
	Symbol_sp exportYourself(bool doit=true);

	void dump();

        void __write__(T_sp stream) const; // in write_symbol.cc

	string __repr__() const;
	string __str__() { return _rep_(this);};
//	string description() const { return "Symbol("+this->_Value+")";};

    public: // ctor/dtor for classes with shared virtual base
	/*! Special constructor used when starting up the Lisp environment */
	explicit Symbol_O(string const& name);
	/*! Used to finish setting up symbol when created with the above constructor */
	void finish_setup(Package_sp pkg, bool exportp);

    public:

	explicit Symbol_O();
	virtual ~Symbol_O();

    };

    T_sp af_symbolValue(const Symbol_sp sym);
    Str_sp af_symbolName(Symbol_sp sym);
    Package_sp af_symbolPackage(Symbol_sp sym);
    Function_sp af_symbolFunction(Symbol_sp sym);
    bool af_boundp(Symbol_sp sym);



};
TRANSLATE(core::Symbol_O);
template<> struct gctools::GCInfo<core::Symbol_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true; // old=false
    static bool constexpr Atomic = false;
};



namespace core {
    template <class OT>
    class SymbolMap : public gctools::SmallMap<Symbol_sp,gctools::smart_ptr<OT>> {
        typedef gctools::SmallMap<Symbol_sp,gctools::smart_ptr<OT>> BaseType;
    public:
        typedef typename BaseType::iterator iterator;
        typedef typename BaseType::const_iterator const_iterator;
    };

};



#endif //]
