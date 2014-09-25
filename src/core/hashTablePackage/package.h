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
#include "multipleValues.fwd.h"
#include "holder.h"
#include "wrappers.h"

namespace core {


    SMART(Package );
    class Package_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(CorePkg,Package_O,"Package");
	DECLARE_INIT();
public: // virtual functions inherited from Object
	void	initialize();
	void	archiveBase(ArchiveP node);
	string	__repr__() const;

private: // instance variables
	string		_Name;
	HashTableEql_sp 	_InternalSymbols;
	HashTableEql_sp 	_ExternalSymbols;
//	StringMap<Symbol_O>	_InternalSymbols;
//	StringMap<Symbol_O>	_ExternalSymbols;
	StringMap<Symbol_O>	_ShadowingSymbols;
	Set<Package_O>	_UsingPackages;
	bool		_KeywordPackage;
	bool		_AmpPackage;

public:
	typedef StringMap<Symbol_O>::iterator	symbolIterator;
	typedef StringMap<Symbol_O>::const_iterator	const_symbolIterator;
public:	// Creation class functions
    static Package_sp create(Lisp_sp e,const string& p);
    public:
	/*! Very low level - add to internal symbols unless keyword
	  package, in that case add to external symbols */
	void _add_symbol_to_package(Symbol_sp sym);

public:

	bool packageP() const {return this->notNil();};

	string packageName() const { return this->_Name;};

	MultipleValues_sp packageHashTables() const;

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

//	string allSymbols();

	/*! support for CLHS::shadow */
	bool shadow(Cons_sp listOfSymbolNames);

//	bool areThereNameCollisions(Package_sp otherPackage);

	string getName() { return this->_Name; };
	void setName(const string& n) { this->_Name = n; };

	bool isExported(Symbol_sp sym);

	/*! See CLHS:export function */
	void _export(Cons_sp listOfSymbols);

	/*! Return the symbol if we contain it directly */
	MultipleValues_sp findSymbolDirectlyContained(Bignum_sp nameKey) const;
	
	MultipleValues_sp findSymbol(Bignum_sp nameKey) const;

	/*! Return the (values symbol [:inherited,:external,:internal])
	 */
	MultipleValues_sp findSymbol(const string& name) const;

//	MultipleValues_sp findSymbol(const string& symbolName);


		/*! Return the Symbol if we contain it 
		 * and create it and return it if we don't
		 */
	MultipleValues_sp intern(const string& symbolName);

	/*! Remove the symbol from the package */
	bool unintern(Symbol_sp sym );

	Cons_sp packageUseList();

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

	/*! Dump all the symbols to stdout */
	void dumpSymbols();

	/*! Return the External(HashTable), Internal(HashTable) and UseList(list) */
	MultipleValues_sp hashTables() const;



public:
	Package_O( const Package_O& ss ); //!< Copy constructor

	DEFAULT_CTOR_DTOR(Package_O);
    };


};
TRANSLATE(core::Package_O);
#endif //]
