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
#ifndef Package_H //[
#define Package_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/hashTable.fwd.h>
#include <clasp/core/bignum.fwd.h>
#include <clasp/core/holder.h>
#include <clasp/core/wrappers.h>

namespace core {

SMART(Package);
class Package_O : public General_O {
  LISP_CLASS(core, ClPkg, Package_O, "Package",General_O);
  friend T_sp cl__unexport(Symbol_sp sym, Package_sp package);
  friend T_sp cl__delete_package(T_sp pobj);
public: // virtual functions inherited from Object
  void initialize();
  string __repr__() const;
public: // instance variables
  SimpleString_sp _Name;
  HashTableEqual_sp _InternalSymbols;
  HashTableEqual_sp _ExternalSymbols;
  HashTableEq_sp _Shadowing;
  gctools::Vec0<Package_sp> _UsingPackages;
  gctools::Vec0<Package_sp> _PackagesUsedBy;
  bool _KeywordPackage;
  bool _AmpPackage;
  bool _ActsLikeKeywordPackage;
  List_sp _Nicknames;
public: // Creation class functions
  static Package_sp create(const string &p);

public:
  /*! Very low level - add to internal symbols unless keyword
	  package, in that case add to external symbols */
  void add_symbol_to_package(SimpleString_sp nameKey, Symbol_sp sym, bool exportp = false);
  void bootstrap_add_symbol_to_package(const char *symName, Symbol_sp sym, bool exportp = false, bool shadowp = false);

private:
  // This returns a NULL smart_ptr if it doesn't find a conflict
  // so that it can be used within the expression of an if statement
  Package_sp export_conflict_or_NULL(SimpleString_sp nameKey, Symbol_sp sym);

public:
  string packageName() const;

  T_mv packageHashTables() const;

  void setNicknames(List_sp nicknames) { this->_Nicknames = nicknames; };
  List_sp getNicknames() const { return this->_Nicknames; };
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

  void setKeywordPackage(bool b) { this->_KeywordPackage = b; };
  bool isKeywordPackage() const { return this->_KeywordPackage; };
  // Cando makes a package that acts like the keyword package (symbol values are symbols and all symbols extern)
  void setActsLikeKeywordPackage(bool b) { this->_ActsLikeKeywordPackage = b; };
  bool actsLikeKeywordPackage() const { return this->_KeywordPackage || this->_ActsLikeKeywordPackage; };

  string allSymbols();

  /*! support for CLHS::shadow */
  bool shadow(List_sp listOfSymbolNames);

  /*! support for CLHS::shadow */
  bool shadow(String_sp sym);

  //	bool areThereNameCollisions(Package_sp otherPackage);

  string getName() const;
  void setName(const string &n);

  bool isExported(Symbol_sp sym);

  /*! See CLHS:export function */
  void _export2(Symbol_sp sym);

  /*! Return the symbol if we contain it directly */
  Symbol_mv findSymbolDirectlyContained(String_sp nameKey) const;

  Symbol_mv findSymbol_SimpleString(SimpleString_sp nameKey) const;

  /*! Return the (values symbol [:inherited,:external,:internal])
	 */
  Symbol_mv findSymbol(const string &name) const;
  Symbol_mv findSymbol(String_sp name) const;

  //	T_mv findSymbol(const string& symbolName);

  /*! Return the Symbol if we contain it 
		 * and create it and return it if we don't
		 */
  T_mv intern(SimpleString_sp symbolName);

  /*! Remove the symbol from the package */
  bool unintern(Symbol_sp sym);

  List_sp packageUseList();
  List_sp packageUsedByList();

  /*! Import the symbols into this package - see CLHS */
  void import(List_sp symbols);

  /*! Shadow import the symbols into this package - see CLHS */
  void shadowingImport(List_sp listOfSymbols);

  /*! Return a list of all shadowing symbols */
  List_sp shadowingSymbols() const;

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
  void mapExternals(KeyValueMapper *mapper);

  /*! Map over the Internal key/value pairs */
  void mapInternals(KeyValueMapper *mapper);

public:
  // Not default constructable
  Package_O() : _Nicknames(_Nil<T_O>()), _ActsLikeKeywordPackage(false){};
  virtual ~Package_O(){};
};

T_mv cl__find_symbol(String_sp symbolName, T_sp packageDesig);
};
#endif //]
