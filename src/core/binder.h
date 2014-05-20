#ifndef	_core_binder_H //[
#define _core_binder_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "symbol.h"
#include "vectorObjects.h"
#include "holder.h"
//#include "stringSet.fwd.h"
#include "environment.fwd.h"
#include "activationFrame.fwd.h"
#include "cons.h"
#include "binder.fwd.h"

namespace core
{

    SMART(ObjectDictionary);
    SMART(Name);



    SMART(Binder);
    class Binder_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,CorePkg,Binder_O,"Binder");
	void initialize();

#if defined(OLD_SERIALIZE)
	DECLARE_SERIALIZE();
#endif // defined(OLD_SERIALIZE)
    public:
	void archiveBase(ArchiveP node);
    private:
        HashTableEq_sp          _Bindings;
        VectorObjects_sp        _Values;
    public:

	uint size() { return this->_Values->length();};

	T_sp indexed_value(int idx) const { return this->_Values->operator[](idx); };
	void erase();
	T_sp	extend(Symbol_sp sym, T_sp val);
//	void		update(Symbol_sp sym, T_sp val);
	T_sp	lookup(Symbol_sp sym) const;
	T_sp	lookupSymbol(Symbol_sp sym) const {return this->lookup(sym);};
	T_sp	lookup(const string& package, const string& symStr) const;

	bool		contains(Symbol_sp sym) const;
        //	const_iterator	find(Symbol_sp sym) const;
	// iterator	find(Symbol_sp sym);

	string		summaryOfContents() const;

#if 0
	bool	canRender() { return true; };
	Render_sp rendered(Cons_sp options);
#endif
	/*! Return true if this binder contains the symbol
	 * that you get when you search for the kw
	 */
	bool		containsSymbolFromString(const string& kw);
	T_sp	valueSymbolFromString(const string& kw);

	/*! Return the value for the symbol/string 
	 * or the default
	 */
	T_sp valueOrDefault(const string& kw, T_sp defVal);
	bool boolValueOrDefault(const string& kw, bool defVal);
	int  intValueOrDefault(const string& kw, int defVal);
	int  intValueOrDefault(Symbol_sp, int defVal);
	string stringValueOrDefault(const string& kw, const string& defVal);
	/* Set the value if provided otherwise do nothing.
	 * Return true if the value was provided
	 */
	bool setBoolValueIfAvailable(bool& dest, const string& kw);
	bool setIntValueIfAvailable(int& dest, const string& kw);
	bool setStringValueIfAvailable(string& dest, const string& kw);



	DEFAULT_CTOR_DTOR(Binder_O);
    };

};
    

TRANSLATE(core::Binder_O);

#endif //]


