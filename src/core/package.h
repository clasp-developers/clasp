/*
    File: package.h
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
#ifndef	Package_H //[
#define Package_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "hashTable.fwd.h"
#include "bignum.fwd.h"
#include "holder.h"
#include "wrappers.h"

namespace core {



    SMART(Package );
    class Package_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,ClPkg,Package_O,"Package");
public: // virtual functions inherited from Object
	void	initialize();
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
	string	__repr__() const;

GCPRIVATE: // instance variables
        gctools::gcstring  _Name;
	HashTableEql_sp 	_InternalSymbols;
	HashTableEql_sp 	_ExternalSymbols;
	HashTableEqual_sp       _ShadowingSymbols;
        gctools::Vec0<Package_sp>	_UsingPackages;
        gctools::Vec0<Package_sp>       _PackagesUsedBy;
	bool		_KeywordPackage;
	bool		_AmpPackage;
	Cons_sp		_Nicknames;
public:	// Creation class functions
    static Package_sp create(const string& p);
    public:
	/*! Very low level - add to internal symbols unless keyword
	  package, in that case add to external symbols */
	void add_symbol_to_package(const char* symName, Symbol_sp sym, bool exportp=false);

public:

	string packageName() const { return this->_Name.asStdString();};

	T_mv packageHashTables() const;

	void setNicknames(Cons_sp nicknames) { this->_Nicknames = nicknames;};
	Cons_sp getNicknames() const { return this->_Nicknames;};
#if 0
	symbolIterator beginExternals() { return this->_ExternalSymbols.begin();};
	symbolIterator endExternals() { return this->_ExternalSymbols.end();};

	const_symbolIterator beginExternals() const { return this->_ExternalSymbols.begin();};
	const_symbolIterator endExternals() const { return this->_ExternalSymbols.end();};

	symbolIterator beginInternals() { return this->_InternalSymbols.begin();};
	symbolIterator endInternals() { return this->_InternalSymbols.end();};

	const_symbolIterator beginInternals() const { return this->_InternalSymbols.begin();};
	const_symbolIterator endInternals() const { return this->_InternalSymbols.end();};
#endif


	void setKeywordPackage(bool b) { this->_KeywordPackage = b;};
	bool isKeywordPackage() { return this->_KeywordPackage;};

	string allSymbols();

	/*! support for CLHS::shadow */
	bool shadow(Cons_sp listOfSymbolNames);

	/*! support for CLHS::shadow */
	bool shadow(Str_sp sym);

//	bool areThereNameCollisions(Package_sp otherPackage);

        string getName() const { return this->_Name.asStdString(); };
	void setName(const string& n) { this->_Name = n; };

	bool isExported(Symbol_sp sym);

	/*! See CLHS:export function */
	void _export(Cons_sp listOfSymbols);

	/*! Return the symbol if we contain it directly */
	Symbol_mv findSymbolDirectlyContained(Bignum_sp nameKey) const;
	
	Symbol_mv findSymbol(Bignum_sp nameKey) const;

	/*! Return the (values symbol [:inherited,:external,:internal])
	 */
	Symbol_mv findSymbol(const string& name) const;

//	T_mv findSymbol(const string& symbolName);


		/*! Return the Symbol if we contain it 
		 * and create it and return it if we don't
		 */
	T_mv intern(const string& symbolName);

	/*! Remove the symbol from the package */
	bool unintern(Symbol_sp sym );

	Cons_sp packageUseList();
        Cons_sp packageUsedByList();

	/*! Import the symbols into this package - see CLHS */
	void import( Cons_sp symbols );

	/*! Shadow import the symbols into this package - see CLHS */
	void shadowingImport( Cons_sp listOfSymbols );

	/*! Return a list of all shadowing symbols */
	Cons_sp shadowingSymbols() const;


		/*! Use the package, if there are any overlapping symbols
		 * then don't use the package and return false.
		 * If you use the package return true.
		 */
	bool usePackage(Package_sp usePackage);

		/*! Unuse the package, the reverse of usePackage
		 */
	bool unusePackage(Package_sp usePackage);

	/*! Return true if we are using the package */
	bool usingPackageP(Package_sp pkg) const;

	/*! Dump all the symbols to stdout */
	void dumpSymbols();

	/*! Return the External(HashTable), Internal(HashTable) and UseList(list) */
	T_mv hashTables() const;


	/*! Map over the External key/value pairs */
	void mapExternals(KeyValueMapper* mapper);

	/*! Map over the Internal key/value pairs */
	void mapInternals(KeyValueMapper* mapper);




public:
	Package_O() : _Nicknames(_Nil<Cons_O>()) {};
	virtual ~Package_O() {};
    };


    struct FindConflicts : public KeyValueMapper
    {
    public:
	set<string> _conflicts;
	Package_sp  _me;
	FindConflicts(Package_sp me)
	{
	    this->_me = me;
	}


	virtual bool mapKeyValue(T_sp key, T_sp value)
	{
	    Bignum_sp nameKey = key.as<Bignum_O>();
	    Symbol_sp svalue = value.as<Symbol_O>();
	    
	    Symbol_sp mine;
	    T_sp foundp;
	    {MULTIPLE_VALUES_CONTEXT();
		T_mv values = this->_me->findSymbol(nameKey);
		mine = values.as<Symbol_O>();;
		foundp = values.valueGet(1).as<T_O>();
	    }
	    if ( foundp.notnilp() && mine != svalue )
	    {
		LOG(BF("usePackage conflict - my symbol[%s] : usePackage symbol[%s]")
		    % _rep_(mine) % _rep_(svalue));
		this->_conflicts.insert(svalue->symbolNameAsString());
	    }
	    return true;
	}
    };



    T_mv cl_findSymbol(const string& symbolName, T_sp packageDesig);

};
TRANSLATE(core::Package_O);
#endif //]
