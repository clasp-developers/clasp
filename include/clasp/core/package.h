#pragma once
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

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>
#include <clasp/core/hashTable.fwd.h>
#include <clasp/core/bignum.fwd.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/wrappers.h>

namespace core {

#define WITH_PACKAGE_READ_LOCK(pkg) WITH_READ_LOCK(pkg->_Lock)
#define WITH_PACKAGE_READ_WRITE_LOCK(pkg) WITH_READ_WRITE_LOCK(pkg->_Lock)

SMART(Package);
class Package_O : public General_O {
  LISP_CLASS(core, ClPkg, Package_O, "Package", General_O);
  friend T_sp cl__delete_package(T_sp pobj);
  friend List_sp core__export_conflicts(Package_sp, SimpleString_sp, Symbol_sp);

public: // virtual functions inherited from Object
  void initialize() override;
  string __repr__() const override;
  void __write__(T_sp stream) const override; // in write_ugly.cc

public: // instance variables
  HashTable_sp _InternalSymbols;
  HashTable_sp _ExternalSymbols;
  HashTable_sp _Shadowing;
  SimpleString_sp _Name;
  gctools::Vec0<Package_sp> _UsingPackages;
  gctools::Vec0<Package_sp> _PackagesUsedBy;
  List_sp _Nicknames;
  List_sp _LocalNicknames;
  gctools::Vec0<Package_sp> _Implementors;
  T_sp _Documentation;
#ifdef CLASP_THREADS
  mutable mp::SharedMutex _Lock;
#endif
  std::atomic<uint16_t> _Flags;

public: // Creation class functions
  static Package_sp create(const string& p);
  static Package_sp create(SimpleString_sp name);

public:
  /*! Very low level - add to internal symbols unless keyword
          package, in that case add to external symbols */
  void add_symbol_to_package_no_lock(SimpleString_sp nameKey, Symbol_sp sym, bool exportp = false);
  void add_symbol_to_package(SimpleString_sp nameKey, Symbol_sp sym, bool exportp = false);
  void bootstrap_add_symbol_to_package(const char* symName, Symbol_sp sym, bool exportp = false, bool shadowp = false);

private:
  // Returns a list of packages that will newly conflict.
  List_sp export_conflicts(SimpleString_sp nameKey, Symbol_sp sym);
  // flag masks.
  static const uint16_t flag_zombie = 0x01; // is the package deleted?
  // is it a system package? (unused for now)
  static const uint16_t flag_sys = 0x02;
  // is it locked? (i.e. unintern blocked, etc., not the mutex)
  static const uint16_t flag_lock = 0x04;
  // is this the keyword package?
  static const uint16_t flag_keyword = 0x08;
  static const uint16_t flag_actskw = 0x10;

public:
  string packageName() const;
  SimpleString_sp name() const { return this->_Name; }

  T_mv packageHashTables() const;

  void setNicknames(List_sp nicknames) {
    WITH_PACKAGE_READ_WRITE_LOCK(this);
    this->_Nicknames = nicknames;
  };
  List_sp getNicknames() const {
    WITH_PACKAGE_READ_LOCK(this);
    return this->_Nicknames;
  };

  void setLocalNicknames(List_sp localNicknames) { this->_LocalNicknames = localNicknames; }
  List_sp getLocalNicknames() const { return this->_LocalNicknames; }
  T_sp findPackageByLocalNickname(String_sp);

  string allSymbols();

  /*! support for CLHS::shadow */
  bool shadow(List_sp listOfSymbolNames);

  /*! support for CLHS::shadow */
  bool shadow(String_sp sym);

  /*! support for CLHS::unexport */
  void unexport(Symbol_sp sym);

  void setName(SimpleString_sp newName);

  bool isExported(Symbol_sp sym);

  /*! See CLHS:export function */
  void _export2(Symbol_sp sym);

  /*! Return the symbol if we contain it directly */
  Symbol_mv findSymbolDirectlyContained(String_sp nameKey) const;

  Symbol_mv findSymbol_SimpleString_no_lock(SimpleString_sp nameKey) const;
  Symbol_mv findSymbol_SimpleString(SimpleString_sp nameKey) const;

  /*! Return the (values symbol [:inherited,:external,:internal])
   */
  Symbol_mv findSymbol(const string& name) const;
  Symbol_mv findSymbol(String_sp name) const;

  //	T_mv findSymbol(const string& symbolName);

  /*! Return the Symbol if we contain it
   * and create it and return it if we don't
   */
  T_mv intern(SimpleString_sp symbolName);

  bool unintern_unsafe(Symbol_sp sym);

  /*! Remove the symbol from the package */
  bool unintern(Symbol_sp sym);

  List_sp packageUseList();
  List_sp packageUsedByList();

  void import1(Symbol_sp); // import one symbol
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
  bool unusePackage_no_outer_lock(Package_sp usePackage);
  bool unusePackage_no_inner_lock(Package_sp usePackage);

  void addImplementationPackage(Package_sp);
  void removeImplementationPackage(Package_sp);

  bool usingPackageP_no_lock(Package_sp pkg) const;
  /*! Return true if we are using the package */
  bool usingPackageP(Package_sp pkg) const;

  T_sp documentation() const { return this->_Documentation; }
  void setDocumentation(T_sp docstring) { this->_Documentation = docstring; }

  /*! Dump all the symbols to stdout */
  void dumpSymbols();

  /*! Return the External(HashTable), Internal(HashTable) and UseList(list) */
  T_mv hashTables() const;

  /*! Map over the External key/value pairs */
  void mapExternals(KeyValueMapper* mapper);

  /*! Map over the Internal key/value pairs */
  void mapInternals(KeyValueMapper* mapper);
private:
  inline uint16_t flags() const { return _Flags.load(std::memory_order_relaxed); }
  inline bool getFlag(uint16_t n) const { return !!(flags() & n); }
  inline void setFlag(bool flag, uint16_t n) {
    if (flag) _Flags.fetch_or(n, std::memory_order_relaxed);
    else _Flags.fetch_and(~n, std::memory_order_relaxed);
  }
public:
  void setSystemPackageP(bool value) { setFlag(value, flag_sys); }
  bool getSystemPackageP() const { return getFlag(flag_sys); }
  void setLockedP(bool value) { setFlag(value, flag_lock); }
  bool getLockedP() const { return getFlag(flag_lock); }

  // is this package locked? accounting for implementation packages
  // (use this instead of getLockedP most of the time)
  bool lockedP() const;

  void setZombieP(bool value) { setFlag(value, flag_zombie); }
  bool getZombieP() { return getFlag(flag_zombie); }
  void setKeywordPackage(bool b) { setFlag(b, flag_keyword); }
  bool isKeywordPackage() const { return getFlag(flag_keyword); }
  // Cando makes a package that acts like the keyword package (symbol values are symbols and all symbols extern)
  void setActsLikeKeywordPackage(bool b) { setFlag(b, flag_actskw); }
  bool actsLikeKeywordPackage() const {
    return isKeywordPackage() || getFlag(flag_actskw);
  }


public:
  // Not default constructable
  Package_O()
      : _Flags(0), _Nicknames(nil<T_O>()), _LocalNicknames(nil<T_O>()), _Documentation(nil<T_O>()),
        _Lock(PACKAGE__NAMEWORD){
    // by default, packages implement themselves.
    // this can be changed later.
    this->_Implementors.push_back(this->asSmartPtr());
  };

  virtual void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
    if (snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::LoadOp) {
      //      printf("%s:%d:%s About to initialize an mp::SharedMutex for a Package_O object\n", __FILE__, __LINE__, __FUNCTION__ );
      new (&this->_Lock) mp::SharedMutex(PACKAGE__NAMEWORD);
    }
  }

};

T_mv cl__find_symbol(String_sp symbolName, T_sp packageDesig);
T_sp cl__delete_package(T_sp pobj);
}; // namespace core
