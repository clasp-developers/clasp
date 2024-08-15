
/*
    File: package.cc
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
// #define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#ifdef DEFINE_CL_SYMBOLS
#include <clasp/core/allClSymbols.h>
#endif
#include <clasp/core/common.h>
#include <clasp/core/symbol.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/package.h>
#include <clasp/core/numberToString.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/bignum.h>
#include <clasp/core/array.h>
#include <clasp/core/debugger.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/evaluator.h> // for eval::funcall
#include <clasp/core/cons.h>
#include <clasp/core/lispList.h>
// last include is wrappers.h
#include <clasp/core/wrappers.h>

namespace core {

DOCGROUP(clasp);
CL_DEFUN Package_sp core__package_lock(T_sp pkg) { 
  Package_sp package = coerce::packageDesignator(pkg); 
  package->setLockedP(true);
  return package; 
}
CL_DEFUN Package_sp core__package_unlock(T_sp pkg) {
  Package_sp package = coerce::packageDesignator(pkg);
  package->setLockedP(false);
  return package;
}
CL_DEFUN bool core__package_locked_p(T_sp pkg) {
  Package_sp package = coerce::packageDesignator(pkg);
  return package->getLockedP();
}
CL_LAMBDA(package new-name &optional nick-names);
CL_DECLARE();
CL_DOCSTRING(R"dx(renamePackage)dx");
DOCGROUP(clasp);
CL_DEFUN Package_sp cl__rename_package(T_sp pkg, T_sp newNameDesig, T_sp nickNameDesigs) {
  Package_sp package = coerce::packageDesignator(pkg);
  SimpleString_sp newName = coerce::packageNameDesignator(newNameDesig);
  List_sp nickNames = coerce::listOfStringDesignators(nickNameDesigs);
  bool ignore_lock = false;
 retry:
  if (!ignore_lock && package->lockedP())
      goto package_lock_violation;
  // Remove the old names from the Lisp system
  _lisp->unmapNameToPackage(package->_Name);
  for (auto cur : package->getNicknames()) {
    _lisp->unmapNameToPackage(gc::As<String_sp>(oCar(cur)));
  }
  // Set up the new names
  package->setName(newName);
  _lisp->mapNameToPackage(newName, package);
  for (auto cur : nickNames) {
    _lisp->mapNameToPackage(gc::As<String_sp>(oCar(cur)), package);
  };
  package->setNicknames(nickNames);
  return package;
 package_lock_violation:
  CEpackage_lock_violation(package, "renaming ~s", 1, newNameDesig);
  goto retry;
};

CL_LAMBDA(pkg);
CL_DECLARE();
CL_DOCSTRING(R"dx(packageNicknames)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__package_nicknames(T_sp pkg) {
  Package_sp package = coerce::packageDesignator(pkg);
  return package->getNicknames();
};

CL_LAMBDA(pkg nickname);
CL_DECLARE();
CL_DOCSTRING(R"dx(Adds the nickname <nickname> to package)dx");
CL_DOCSTRING_LONG(R"dx(Returns new nicknames. A package error is signalled,
if package does not exist, or nickname is already in use)dx")
DOCGROUP(clasp);
CL_DEFUN T_sp ext__package_add_nickname(T_sp pkg, T_sp nick) {
  Package_sp package = coerce::packageDesignator(pkg);
  String_sp nickname = coerce::stringDesignator(nick);
  if (package->lockedP()) {
    CEpackage_lock_violation(package, "adding nickname ~s", 1, nickname);
  }
  T_sp packageUsingNickName = _lisp->findPackage(nickname);
  if (packageUsingNickName.notnilp()) {
    if (Package_sp pkg = packageUsingNickName.asOrNull<Package_O>())
      SIMPLE_PACKAGE_ERROR_2_args("Package nickname[~a] is already being used by package[~a]", _rep_(nickname),
                                  _rep_(pkg->name()));
    else
      SIMPLE_PACKAGE_ERROR("Package nickname[~a] is already being used", _rep_(nickname));
  } else {
    _lisp->mapNameToPackage(nickname, package);
    package->setNicknames(Cons_O::create(nickname, package->getNicknames()));
    return package->getNicknames();
  }
};

CL_LAMBDA(pkg nickname);
CL_DECLARE();
CL_DOCSTRING(R"dx(Removes the nickname <nickname> from package)dx");
CL_DOCSTRING_LONG(R"dx(Returns nil, if nickname does not belong to package.
A package error is signalled, if package does not exist)dx")
DOCGROUP(clasp);
CL_DEFUN T_sp ext__package_remove_nickname(T_sp pkg, T_sp nick) {
  Package_sp package = coerce::packageDesignator(pkg);
  if (package->lockedP()) {
    CEpackage_lock_violation(package, "removing nickname ~s", 1,
                             coerce::stringDesignator(nick));
  }
  unlikely_if(package->getNicknames().nilp()) return nil<T_O>();
  String_sp nickname = coerce::stringDesignator(nick);
  // verify if nickname is really nicknames of this package
  if (_lisp->findPackage(nickname) == package) {
    _lisp->unmapNameToPackage(nickname);
    package->setNicknames(remove_equal(nickname, package->getNicknames()));
    return package->getNicknames();
  } else
    return nil<T_O>();
};

CL_LAMBDA(pkg);
CL_DECLARE();
CL_DOCSTRING(R"dx(Grab the (local-nickname . package) alist without locking. No coercion.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__package_local_nicknames_internal(Package_sp package) { return package->getLocalNicknames(); }

CL_LISPIFY_NAME("core:package-local-nicknames-internal");
CL_LAMBDA(nicks pkg);
CL_DECLARE();
CL_DOCSTRING(R"dx(Set the local nicknames of PKG to be NICKS. Internal, unlocked, no coercion. Be careful.)dx");
DOCGROUP(clasp);
CL_DEFUN_SETF void set_package_local_nicknames_internal(T_sp nicks, Package_sp package) { package->setLocalNicknames(nicks); }

// FIXME: Maybe we can just grab the lock in CL?
CL_LAMBDA(pkg thunk);
CL_DECLARE();
CL_DOCSTRING(R"dx(Call THUNK while holding the read lock for PKG, and return the result.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__call_with_package_read_lock(Package_sp pkg, Function_sp thunk) {
  WITH_PACKAGE_READ_LOCK(pkg);
  return eval::funcall(thunk);
}

CL_LAMBDA(pkg thunk);
CL_DECLARE();
CL_DOCSTRING(R"dx(Call THUNK while holding the read-write lock for PKG, and return the result.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__call_with_package_read_write_lock(Package_sp pkg, Function_sp thunk) {
  WITH_PACKAGE_READ_WRITE_LOCK(pkg);
  return eval::funcall(thunk);
}

CL_LAMBDA(symbol &optional (package *package*));
CL_DECLARE();
CL_DOCSTRING(R"dx(unintern)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__unintern(Symbol_sp sym, T_sp packageDesig) {
  Package_sp pkg = coerce::packageDesignator(packageDesig);
  return pkg->unintern(sym);
};

CL_LAMBDA(symbol-name &optional (package *package*));
CL_DECLARE();
CL_DOCSTRING(R"dx(findSymbol)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__find_symbol(String_sp symbolName, T_sp packageDesig) {
  Package_sp package = coerce::packageDesignator(packageDesig);
  SimpleString_sp simple_symbol_name = coerce::simple_string(symbolName);
  return package->findSymbol_SimpleString(simple_symbol_name);
};

CL_LAMBDA("package-name &key nicknames (use (list \"CL\"))");
CL_DECLARE();
CL_DOCSTRING(R"dx(make_package)dx");
DOCGROUP(clasp);
CL_DEFUN Package_sp cl__make_package(T_sp package_name_desig, List_sp nick_names, List_sp use_packages) {
  ql::list use;
  for (auto u : use_packages) {
    use << coerce::packageDesignator(oCar(u));
  }
  return _lisp->makePackage(coerce::simple_string(package_name_desig), nick_names, use.cons());
}

/*
  __BEGIN_DOC(candoScript.general.listAllPackages,listAllPackages)
  \scriptCmdRet{listAllPackages}{}{Text::packageName}

  Return a list of all packages.
  __END_DOC
*/

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(listAllPackages)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__list_all_packages() { return _lisp->allPackagesAsCons(); }

/*
  __BEGIN_DOC(candoScript.general.usePackage,usePackage)
  \scriptCmdRet{usePackage}{}{Symbol::packageName}

  Use the package. Return true if we used it.
  __END_DOC
*/

CL_LAMBDA(packages-to-use-desig &optional (package-desig *package*));
CL_DECLARE();
CL_DOCSTRING(R"dx(See CLHS use-package)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__use_package(T_sp packages_to_use_desig, T_sp package_desig) {
  List_sp packages_to_use = coerce::listOfPackageDesignators(packages_to_use_desig);
  Package_sp package = coerce::packageDesignator(package_desig);
  for (auto cur : packages_to_use) {
    Package_sp package_to_use = gc::As<Package_sp>(oCar(cur));
    package->usePackage(package_to_use);
  }
  return _lisp->_true();
}

CL_LAMBDA(packages-to-unuse-desig &optional (package-desig *package*));
CL_DECLARE();
CL_DOCSTRING(R"dx(SeeCLHS unuse-package)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__unuse_package(T_sp packages_to_unuse_desig, T_sp package_desig) {
  List_sp packages_to_unuse = coerce::listOfPackageDesignators(packages_to_unuse_desig);
  Package_sp package = coerce::packageDesignator(package_desig);
  for (auto cur : packages_to_unuse) {
    Package_sp package_to_unuse = gc::As<Package_sp>(oCar(cur));
    package->unusePackage(package_to_unuse);
  }
  return _lisp->_true();
}

CL_LAMBDA(pkg);
CL_DOCSTRING(R"dx(SeeCLHS delete-package)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__delete_package(T_sp pobj) {
  // clhs http://www.lispworks.com/documentation/HyperSpec/Body/f_del_pk.htm
  // If the package designator is a name that does not currently name a package,
  // a correctable error of type package-error is signaled. If correction is attempted,
  // no deletion action is attempted; instead, delete-package immediately returns nil.

  T_sp potentialPkg = coerce::packageDesignatorNoError(pobj);
  unlikely_if(potentialPkg.nilp()) {
    CEpackage_error("Package with designator ~S not found. Cannot delete it.", "Ignore error and continue.", nil<T_O>(), 1, pobj);
    return nil<T_O>();
  }
  Package_sp pkg = gc::As<Package_sp>(potentialPkg);
  if (pkg->lockedP()) {
    CEpackage_lock_violation(pkg, "deleting package", 0);
  }

  if (pkg->getZombieP()) { // already deleted
    return nil<T_O>();
  }

  // If package is used by other packages, a correctable error of type package-error is signaled.
  // If correction is attempted, unuse-package is effectively called to remove any dependencies,
  // causing package's external symbols to cease being accessible to those packages that use package.
  // delete-package then deletes package just as it would have had there been no packages that used it.

  unlikely_if(pkg->_PackagesUsedBy.size() > 0) CEpackage_error(
      "Package with designator ~S is used by at least one other package:", "Remove dependency in other packages", pkg, 1, pkg);
  /*
    2) Now remove the package from the other packages that use it
       and empty the package.
  */

  // Need to remove the nicknames, since package is not fully deleted
  // do this before the WITH_PACKAGE_READ_WRITE_LOCK
  for (auto cur : pkg->getNicknames()) {
    _lisp->unmapNameToPackage(gc::As<String_sp>(oCar(cur)));
  }
  pkg->setNicknames(nil<T_O>());

  // need to grab before the readwriteLock
  List_sp usedList = pkg->packageUseList();
  List_sp usedByList = pkg->packageUsedByList();

  WITH_PACKAGE_READ_WRITE_LOCK(pkg);
  for (auto pi : usedList) {
    Package_sp piLocal = gc::As<Package_sp>(CONS_CAR(pi));
    pkg->unusePackage_no_outer_lock(piLocal);
  }

  for (auto pi : usedByList) {
    Package_sp piLocal = gc::As<Package_sp>(CONS_CAR(pi));
    piLocal->unusePackage_no_inner_lock(pkg);
  }
  pkg->_InternalSymbols->mapHash([pkg](T_sp key, T_sp tsym) {
    Symbol_sp sym = gc::As<Symbol_sp>(tsym);
    sym->remove_package(pkg);
  });
  pkg->_InternalSymbols->clrhash();
  pkg->_ExternalSymbols->mapHash([pkg](T_sp key, T_sp tsym) {
    Symbol_sp sym = gc::As<Symbol_sp>(tsym);
    sym->remove_package(pkg);
  });
  pkg->_ExternalSymbols->clrhash();
  pkg->_Shadowing->clrhash();
  String_sp package_name = pkg->_Name;
  pkg->_Name = SimpleBaseString_O::make("");
  _lisp->remove_package(package_name);
  return _lisp->_true();
}

CL_LAMBDA(package-desig);
CL_DECLARE();
CL_DOCSTRING(R"dx(See CLHS package_shadowing_symbols)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__package_shadowing_symbols(T_sp package_desig) {
  Package_sp package = coerce::packageDesignator(package_desig);
  return package->shadowingSymbols();
}

/*
  __BEGIN_DOC(candoScript.general.import,import)
  \scriptCmdRet{import}{}{symbols &optional package}

  Import the symbols into the (package) or the current package.
  __END_DOC
*/
CL_LAMBDA(symbols-desig &optional (package-desig *package*));
CL_DECLARE();
CL_DOCSTRING(R"dx(See CLHS: import)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__import(T_sp symbols_desig, T_sp package_desig) {
  List_sp symbols = coerce::listOfSymbols(symbols_desig);
  Package_sp package = coerce::packageDesignator(package_desig);
  package->import(symbols);
  return Values(_lisp->_true());
}

CL_LAMBDA(symbol-names-desig &optional (package-desig *package*));
CL_DOCSTRING(R"dx(See CLHS: shadow)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__shadow(T_sp symbol_names_desig, T_sp package_desig) {
  List_sp symbolNames = coerce::listOfStringDesignators(symbol_names_desig);
  Package_sp package = coerce::packageDesignator(package_desig);
  package->shadow(symbolNames);
  return Values(_lisp->_true());
}

CL_LAMBDA(package);
CL_DOCSTRING(R"dx(Retrieve the documentation of a package.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__package_documentation(Package_sp package) { return package->documentation(); }

CL_LISPIFY_NAME("core:package-documentation");
CL_LAMBDA(documentation package);
CL_DOCSTRING(R"dx(Set the documentation of a package.)dx");
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp set_package_documentation(T_sp documentation, Package_sp package) {
  package->setDocumentation(documentation);
  return documentation;
}

CL_LAMBDA(symbol-names-desig &optional (package-desig *package*));
CL_DECLARE();
CL_DOCSTRING(R"dx(See CLHS: shadowing-import)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__shadowing_import(T_sp symbol_names_desig, T_sp package_desig) {
  List_sp symbolNames = coerce::listOfSymbols(symbol_names_desig);
  Package_sp package = coerce::packageDesignator(package_desig);
  package->shadowingImport(symbolNames);
  return Values(_lisp->_true());
}

std::atomic<uint64_t> static_gentemp_counter;
CL_LAMBDA(&optional (prefix "T") (package *package*));
CL_DECLARE();
CL_DOCSTRING(R"dx(See CLHS gentemp)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__gentemp(T_sp prefix, T_sp package_designator) {
  Package_sp pkg = coerce::packageDesignator(package_designator);
  StrNs_sp ss;
  if (prefix.nilp()) {
    // Should signal an error of type type-error if prefix is not a string.
    TYPE_ERROR(prefix, cl::_sym_string);
  } else if (cl__stringp(prefix)) {
    String_sp sprefix = gc::As_unsafe<String_sp>(prefix);
    ss = gc::As_unsafe<StrNs_sp>(
        core__make_vector(sprefix->element_type(), sprefix->length() + 8, true, clasp_make_fixnum(sprefix->length())));
    ss->unsafe_setf_subseq(0, sprefix->length(), sprefix);
  } else {
    TYPE_ERROR(prefix, cl::_sym_string);
  }
  T_sp retval;
  size_t fillPointer = ss->fillPointer();
#define GENTEMP_TRIES 1000000
  size_t tries = GENTEMP_TRIES;
  MultipleValues& mvn = core::lisp_multipleValues();
  while (1) {
    ++static_gentemp_counter;
    core__integer_to_string(ss, Integer_O::create<uint64_t>(static_gentemp_counter), clasp_make_fixnum(10), false, false);
    T_mv rmv = pkg->findSymbol(ss);
    if (mvn.valueGet(1, rmv.number_of_values()).nilp()) {
      retval = pkg->intern(ss->asMinimalSimpleString());
      return retval;
    }
    ss->fillPointerSet(fillPointer);
    --tries;
    if (tries == 0) {
      SIMPLE_ERROR("gentemp tried {} times to generate a unique symbol and then gave up", GENTEMP_TRIES);
    }
  }
};

CL_LAMBDA(package-designator);
CL_DECLARE();
CL_DOCSTRING(R"dx(package_use_list)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__package_use_list(T_sp package_designator) {
  Package_sp pkg = coerce::packageDesignator(package_designator);
  return pkg->packageUseList();
};

CL_LAMBDA(package-designator);
CL_DECLARE();
CL_DOCSTRING(R"dx(packageUsedByList)dx");
DOCGROUP(clasp);
CL_DEFUN List_sp cl__package_used_by_list(T_sp pkgDesig) {
  Package_sp pkg = coerce::packageDesignator(pkgDesig);
  return pkg->packageUsedByList();
};

CL_LAMBDA(package-designator);
CL_DECLARE();
CL_DOCSTRING(R"dx(packageName)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__package_name(T_sp pkgDesig) {
  Package_sp pkg = coerce::packageDesignator(pkgDesig);
  if (pkg->getZombieP()) // deleted
    return nil<T_O>();
  else return pkg->name();
};

SYMBOL_EXPORT_SC_(ClPkg, package_use_list);
SYMBOL_EXPORT_SC_(ClPkg, gentemp);
SYMBOL_EXPORT_SC_(ClPkg, makePackage);
SYMBOL_EXPORT_SC_(ClPkg, listAllPackages);
SYMBOL_EXPORT_SC_(ClPkg, use_package);
SYMBOL_EXPORT_SC_(ClPkg, unuse_package);
SYMBOL_EXPORT_SC_(ClPkg, package_shadowing_symbols);
SYMBOL_EXPORT_SC_(ClPkg, import);
SYMBOL_EXPORT_SC_(ClPkg, shadow);
SYMBOL_EXPORT_SC_(ClPkg, shadowing_import);
SYMBOL_EXPORT_SC_(ClPkg, findSymbol);
SYMBOL_EXPORT_SC_(ClPkg, unintern);
SYMBOL_EXPORT_SC_(CorePkg, import_name_conflict);
SYMBOL_EXPORT_SC_(CorePkg, export_name_conflict);
SYMBOL_EXPORT_SC_(CorePkg, unintern_name_conflict);
SYMBOL_EXPORT_SC_(CorePkg, use_package_name_conflict);
SYMBOL_EXPORT_SC_(CorePkg, package_lock_violation);

Package_sp Package_O::create(const string& name) {
  Package_sp p = Package_O::create();
  p->setName(SimpleBaseString_O::make(name));
  return p;
}
Package_sp Package_O::create(SimpleString_sp name) {
  Package_sp p = Package_O::create();
  p->setName(name);
  return p;
}

void Package_O::initialize() {
  WITH_PACKAGE_READ_WRITE_LOCK(this);
  this->Base::initialize();
  this->_InternalSymbols = HashTableEqual_O::create_default();
  this->_ExternalSymbols = HashTableEqual_O::create_default();
  this->_Shadowing = HashTableEq_O::create_default();
#if 0
  this->_InternalSymbols->setupThreadSafeHashTable();
  this->_ExternalSymbols->setupThreadSafeHashTable();
  this->_Shadowing->setupThreadSafeHashTable();
#endif
}

bool Package_O::lockedP() const {
  return this->getLockedP()
    // if we're in an implementation package, we're good
    && (_lisp->getCurrentPackage() != this->asSmartPtr());
}

string Package_O::packageName() const { return this->_Name->get_std_string(); }

void Package_O::setName(SimpleString_sp n) {
  WITH_PACKAGE_READ_WRITE_LOCK(this);
  this->_Name = n;
};

CL_LISPIFY_NAME("core:PackageHashTables");
CL_DEFMETHOD T_mv Package_O::hashTables() const {
  WITH_PACKAGE_READ_LOCK(this);
  List_sp useList = nil<List_V>();
  for (auto ci : this->_UsingPackages) {
    useList = Cons_O::create(ci, useList);
  }
  return Values(this->_ExternalSymbols, this->_InternalSymbols, useList);
}

string Package_O::__repr__() const {
  WITH_PACKAGE_READ_LOCK(this);
  if (cl::_sym_STARprint_readablySTAR->symbolValue().notnilp()) {
    stringstream ss;
    ss << "#.(CL:FIND-PACKAGE \"" << _rep_(this->_Name) << "\")";
    return ss.str();
  }
  stringstream ss;
  ss << "#<PACKAGE " << _rep_(this->_Name) << ">";
  return ss.str();
}

class PackageMapper : public KeyValueMapper {
public:
  stringstream* ssP;
  string type;
  PackageMapper(const string& t, stringstream* sstr) : ssP(sstr), type(t){};
  virtual bool mapKeyValue(T_sp key, T_sp value) {
    (*(this->ssP)) << type << " " << _rep_(value) << std::endl;
    return true;
  }
};

string Package_O::allSymbols() {
  WITH_PACKAGE_READ_LOCK(this);
  stringstream ss;
  PackageMapper internals("internal", &ss);
  this->_InternalSymbols->lowLevelMapHash(&internals);
  PackageMapper externals("external", &ss);
  this->_ExternalSymbols->lowLevelMapHash(&externals);
  return ss.str();
}

Symbol_mv Package_O::findSymbol_SimpleString_no_lock(SimpleString_sp nameKey) const {
  //  client_validate(nameKey);
  T_mv ei = this->_ExternalSymbols->gethash(nameKey, nil<T_O>());
  //  client_validate(nameKey);
  Symbol_sp val = gc::As<Symbol_sp>(ei);
  MultipleValues& mvn = core::lisp_multipleValues();
  bool foundp = mvn.second(ei.number_of_values()).isTrue();
  if (foundp) {
    //    client_validate(val->_Name);
    LOG("Found it in the _ExternalsSymbols list - returning[{}]", (_rep_(val)));
    return Values(val, kw::_sym_external);
  }
  // There is no need to look further if this is the keyword package
  if (this->isKeywordPackage())
    return Values(nil<T_O>(), nil<T_O>());
  T_mv ej = this->_InternalSymbols->gethash(nameKey, nil<T_O>());
  val = gc::As<Symbol_sp>(ej);
  foundp = mvn.second(ej.number_of_values()).isTrue();
  if (foundp) {
    LOG("Found it in the _InternalSymbols list - returning[{}]", (_rep_(first)));
    return Values(val, kw::_sym_internal);
  }
  {
    for (auto upkg : this->_UsingPackages) {
      LOG("Looking in package[{}]", _rep_(upkg));
      T_mv eu = upkg->_ExternalSymbols->gethash(nameKey, nil<T_O>());
      val = gc::As<Symbol_sp>(eu);
      foundp = mvn.second(eu.number_of_values()).isTrue();
      if (foundp) {
        LOG("Found it in the _ExternalsSymbols list - returning[{}]", (_rep_(val)));
        return Values(val, kw::_sym_inherited);
      }
    }
  }
  return Values(nil<Symbol_O>(), nil<Symbol_O>());
}

Symbol_mv Package_O::findSymbol_SimpleString(SimpleString_sp nameKey) const {
  WITH_PACKAGE_READ_LOCK(this);
  return this->findSymbol_SimpleString_no_lock(nameKey);
}

Symbol_mv Package_O::findSymbol(const string& name) const {
  SimpleBaseString_sp sname = SimpleBaseString_O::make(name);
  return this->findSymbol_SimpleString(sname);
}

Symbol_mv Package_O::findSymbol(String_sp s) const {
  SimpleString_sp ss = coerce::simple_string(s);
  return this->findSymbol_SimpleString(ss);
}

List_sp Package_O::packageUseList() {
  WITH_PACKAGE_READ_LOCK(this);
  List_sp res = nil<List_V>();
  for (auto si : this->_UsingPackages) {
    res = Cons_O::create(si, res);
  }
  return res;
}

List_sp Package_O::packageUsedByList() {
  WITH_PACKAGE_READ_LOCK(this);
  List_sp res = nil<List_V>();
  for (auto si : this->_PackagesUsedBy) {
    res = Cons_O::create(si, res);
  }
  return res;
}

T_mv Package_O::packageHashTables() const {
  WITH_PACKAGE_READ_LOCK(this);
  List_sp usingPackages = nil<List_V>();
  for (auto si : this->_UsingPackages) {
    usingPackages = Cons_O::create(si, usingPackages);
  }
  return Values(this->_ExternalSymbols, this->_InternalSymbols, usingPackages);
}

bool Package_O::usingPackageP_no_lock(Package_sp usePackage) const {
  for (auto it : this->_UsingPackages) {
    if (it == usePackage)
      return true;
  }
  return false;
}

bool Package_O::usingPackageP(Package_sp usePackage) const {
  WITH_PACKAGE_READ_LOCK(this);
  return this->usingPackageP_no_lock(usePackage);
}

// Find a package by local nickname, or return NIL.
T_sp Package_O::findPackageByLocalNickname(String_sp name) {
  WITH_PACKAGE_READ_LOCK(this);
  List_sp nicks = this->getLocalNicknames();
  // This is used by findPackage, which happens pretty early,
  // so we use underlying stuff instead of cl:assoc.
  if (nicks.notnilp()) {
    T_sp pair = nicks.asCons()->assoc(name, nil<T_O>(), cl::_sym_string_EQ_, nil<T_O>());
    if (pair.nilp())
      return pair; // no result
    else
      return oCdr(pair);
  } else
    return nicks;
}

// This is mapped over the external symbols of the package being used.
// The package using is "_me".
// FIXME: We don't lock the other package coherently.
struct FindConflicts : public KeyValueMapper {
public:
  List_sp _conflicts;
  Package_sp _me;
  FindConflicts(Package_sp me) {
    this->_me = me;
    this->_conflicts = nil<List_V>();
  }

  virtual bool mapKeyValue(T_sp key, T_sp value);
};

bool FindConflicts::mapKeyValue(T_sp key, T_sp value) {
  SimpleString_sp nameKey = gc::As_unsafe<SimpleString_sp>(key);
  Symbol_sp svalue = gc::As<Symbol_sp>(value);
  Symbol_mv values = this->_me->findSymbol_SimpleString_no_lock(nameKey);
  Symbol_sp mine = values;
  MultipleValues& mvn = core::lisp_multipleValues();
  T_sp foundp = mvn.second(values.number_of_values());
  if (foundp.notnilp() && mine != svalue) {
    // If mine is in my shadowing list then it's not a conflict
    if (this->_me->_Shadowing->contains(mine))
      return true;
    LOG("usePackage conflict - my symbol[{}] : usePackage symbol[{}]", _rep_(mine), _rep_(svalue));
    this->_conflicts = Cons_O::create(svalue, this->_conflicts);
  }
  return true;
}

bool Package_O::usePackage(Package_sp usePackage) {
  LOG("In usePackage this[{}]  using package[{}]", _rep_(this->name()), _rep_(usePackage->name()));
  bool ignore_lock = false;
  while (true) {
    FindConflicts findConflicts(this->asSmartPtr());
    {
      WITH_PACKAGE_READ_LOCK(this);
      if (!ignore_lock && this->lockedP())
        goto package_lock_violation;
      if (this->usingPackageP_no_lock(usePackage)) {
        LOG("You are already using that package");
        return true;
      }
      usePackage->_ExternalSymbols->lowLevelMapHash(&findConflicts);
      if (cl__length(findConflicts._conflicts) > 0)
        goto name_conflict;
      this->_UsingPackages.push_back(usePackage);
      usePackage->_PackagesUsedBy.push_back(this->asSmartPtr());
    } // release package lock
    return true;
  name_conflict:
    if (core::_sym_use_package_name_conflict->fboundp()) {
      eval::funcall(core::_sym_use_package_name_conflict, this->asSmartPtr(), usePackage, findConflicts._conflicts);
      continue;
    } else
      SIMPLE_ERROR(
          "Conflicts from USE-PACKAGE and name conflict function not yet installed: symbols {} using package {} used package {}",
          _rep_(findConflicts._conflicts), _rep_(this->asSmartPtr()), _rep_(usePackage));

    // That function only returns once conflicts are resolved, so go around
    // again (rechecking for conflicts because multithreading makes this hard)
    // FIXME: This doesn't actually perfectly solve multithreading issues.
  package_lock_violation:
    CEpackage_lock_violation(this->asSmartPtr(), "using ~s", 1,
                             usePackage->name());
    ignore_lock = true;
  }
}

bool Package_O::unusePackage_no_outer_lock(Package_sp usePackage) {
  Package_sp me(this);
  for (auto it = this->_UsingPackages.begin(); it != this->_UsingPackages.end(); ++it) {
    if ((*it) == usePackage) {
      this->_UsingPackages.erase(it);
      for (auto jt = usePackage->_PackagesUsedBy.begin(); jt != usePackage->_PackagesUsedBy.end(); ++jt) {
        if (*jt == me) {
          WITH_PACKAGE_READ_WRITE_LOCK(usePackage);
          usePackage->_PackagesUsedBy.erase(jt);
          return true;
        }
      }
      SIMPLE_ERROR("The unusePackage argument {} is not used by my package {}", _rep_(usePackage->name()), _rep_(this->name()));
    }
  }
  return true;
}

// Used by cl__delete_package
bool Package_O::unusePackage_no_inner_lock(Package_sp usePackage) {
  WITH_PACKAGE_READ_WRITE_LOCK(this);
  Package_sp me(this);
  for (auto it = this->_UsingPackages.begin(); it != this->_UsingPackages.end(); ++it) {
    if ((*it) == usePackage) {
      this->_UsingPackages.erase(it);
      for (auto jt = usePackage->_PackagesUsedBy.begin(); jt != usePackage->_PackagesUsedBy.end(); ++jt) {
        if (*jt == me) {
          usePackage->_PackagesUsedBy.erase(jt);
          return true;
        }
      }
      SIMPLE_ERROR("The unusePackage argument {} is not used by my package {}", _rep_(usePackage->name()), _rep_(this->name()));
    }
  }
  return true;
}

bool Package_O::unusePackage(Package_sp usePackage) {
  bool ignore_lock = false;
 retry:
  {
    WITH_PACKAGE_READ_WRITE_LOCK(this);
    if (!ignore_lock && this->lockedP())
      goto package_lock_violation;
    return this->unusePackage_no_outer_lock(usePackage);
  } // release lock
 package_lock_violation:
  CEpackage_lock_violation(this->asSmartPtr(), "unusing ~s", 1,
                           usePackage->name());
  ignore_lock = true;
  goto retry;
}

// Return a list of packages that will have a conflict if this package
// starts exporting this new symbol. Lock needs to be held around this.
List_sp Package_O::export_conflicts(SimpleString_sp nameKey, Symbol_sp sym) {
  MultipleValues& mvn = core::lisp_multipleValues();
  List_sp conflicts = nil<List_V>();
  for (auto use_pkg : this->_PackagesUsedBy) {
    ASSERT(use_pkg.generalp());
    Symbol_mv x = use_pkg->findSymbol_SimpleString_no_lock(nameKey);
    Symbol_sp xsym = x;
    Symbol_sp status = gc::As<Symbol_sp>(mvn.second(x.number_of_values()));
    if (status.notnilp() && sym != xsym && !use_pkg->_Shadowing->contains(xsym)) {
      conflicts = Cons_O::create(use_pkg, conflicts);
    }
  }
  return conflicts;
}

void Package_O::_export2(Symbol_sp sym) {
  SimpleString_sp nameKey = sym->_Name;
  MultipleValues& mvn = core::lisp_multipleValues();
  List_sp conflicts;
  bool ignore_lock = false;
 start: {
    WITH_PACKAGE_READ_WRITE_LOCK(this);
    if (!ignore_lock && this->lockedP())
      goto package_lock_violation;
    T_mv values = this->findSymbol_SimpleString_no_lock(nameKey);
    Symbol_sp foundSym = gc::As<Symbol_sp>(values);
    Symbol_sp status = gc::As<Symbol_sp>(mvn.second(values.number_of_values()));
    if (status.nilp())
      goto not_accessible;
    else if (foundSym != sym)
      goto already_symbol_same_name;
    else if (status == kw::_sym_external) {
    // Already exported, so EXPORT does nothing.
      return;
    } else {
      conflicts = this->export_conflicts(nameKey, sym);
      if (conflicts.notnilp())
        goto name_conflict;
      else {
      // All problems resolved. Actually do the export.
        if (status == kw::_sym_internal) {
          this->_InternalSymbols->remhash(nameKey);
        }
        this->add_symbol_to_package_no_lock(nameKey, sym, true);
        return;
      }
    }
  } // release package lock
 not_accessible:
  CEpackage_error("The symbol ~S is not accessible from ~S "
                  "and cannot be exported.",
                  "Import the symbol in the package and proceed.", this->asSmartPtr(), 2, sym.raw_(), this->asSmartPtr().raw_());
  // Since the symbol is not accessible, importing cannot cause a name
  // conflict, so we can bypass the usual checks.
  this->add_symbol_to_package(nameKey, sym, false);
  goto start; // start over so that we do conflict checking properly
 already_symbol_same_name:
  FEpackage_error("Cannot export the symbol ~S from ~S,~%"
                  "because there is already a symbol with the same name~%"
                  "in the package.",
                  this->asSmartPtr(), 2, sym.raw_(), this->asSmartPtr().raw_());
  // FEpackage_error never returns.
 name_conflict:
  eval::funcall(core::_sym_export_name_conflict, sym, conflicts);
  // Conflict has been resolved by shadowing-import. Start over.
  goto start;
 package_lock_violation:
  CEpackage_lock_violation(this->asSmartPtr(), "exporting ~s", 1, sym);
  ignore_lock = true;
  goto start;
}

bool Package_O::shadow(String_sp ssymbolName) {
  SimpleString_sp symbolName = coerce::simple_string(ssymbolName);
  Symbol_sp shadowSym, status;
  bool ignore_lock = false;
 start: {WITH_PACKAGE_READ_WRITE_LOCK(this);
    Symbol_mv values = this->findSymbol_SimpleString_no_lock(symbolName);
    shadowSym = values;
    MultipleValues& mvn = core::lisp_multipleValues();
    if (!ignore_lock && this->lockedP()) {
      goto package_lock_violation;
    }
    status = gc::As<Symbol_sp>(mvn.valueGet(1, values.number_of_values()));
    if (status.nilp() || (status != kw::_sym_internal && status != kw::_sym_external)) {
      shadowSym = Symbol_O::create(symbolName);
      shadowSym->makunbound();
      shadowSym->setPackage(this->sharedThis<Package_O>());
      LOG("Created symbol<{}>", _rep_(shadowSym));
      this->add_symbol_to_package_no_lock(shadowSym->symbolName(), shadowSym, false);
    }
    this->_Shadowing->setf_gethash(shadowSym, _lisp->_true());
    return true;
  } // release lock
 package_lock_violation:
  CEpackage_lock_violation(this->asSmartPtr(), "shadowing ~s", 1,
                           ssymbolName);
  ignore_lock = true;
  goto start;
}

bool Package_O::shadow(List_sp symbolNames) {
  for (auto cur : symbolNames) {
    SimpleString_sp name = coerce::simple_string(oCar(cur));
    this->shadow(name);
  }
  return true;
}

void Package_O::unexport(Symbol_sp sym) {
  // Make sure we don't signal an error without releasing the lock first.
  // (The printer will try to access the package name or something, and hang.)
  MultipleValues& mvn = core::lisp_multipleValues();
  bool ignore_lock = false;
 start: {
    WITH_PACKAGE_READ_WRITE_LOCK(this);
    // Don't allow unexporting from locked packages
    if (!ignore_lock && this->lockedP())
      goto package_lock_violation;
    SimpleString_sp nameKey = sym->_Name;
    T_mv values = this->findSymbol_SimpleString_no_lock(nameKey);
    Symbol_sp status = gc::As<Symbol_sp>(mvn.second(values.number_of_values()));
    if (status.nilp()) {
      goto not_accessible;
    } else if (status == kw::_sym_external) {
      this->_ExternalSymbols->remhash(nameKey);
      this->_InternalSymbols->setf_gethash(nameKey, sym);
    }
    return;
  } // release lock
 not_accessible:
  // asSmartPtr.raw_ looks strange, but it fixes the tag.
  FEpackage_error("The symbol ~S is not accessible from ~S "
                  "and cannot be unexported.",
                  this->asSmartPtr(), 2, sym.raw_(), this->asSmartPtr().raw_());
 package_lock_violation:
  CEpackage_lock_violation(this->asSmartPtr(), "unexporting ~s", 1, sym);
  ignore_lock = true;
  goto start;
}

void Package_O::add_symbol_to_package_no_lock(SimpleString_sp nameKey, Symbol_sp sym, bool exportp) {
  if (this->isKeywordPackage() || this->actsLikeKeywordPackage() || exportp) {
    this->_ExternalSymbols->hash_table_setf_gethash(nameKey, sym);
  } else {
    this->_InternalSymbols->hash_table_setf_gethash(nameKey, sym);
  }
  // if the symbol has no home-package, set it to this
  unlikely_if(sym->homePackage().nilp()) sym->setPackage(this->asSmartPtr());
}

void Package_O::add_symbol_to_package(SimpleString_sp nameKey, Symbol_sp sym, bool exportp) {
  WITH_PACKAGE_READ_WRITE_LOCK(this);
  this->add_symbol_to_package_no_lock(nameKey, sym, exportp);
}

void Package_O::bootstrap_add_symbol_to_package(const char* symName, Symbol_sp sym, bool exportp, bool shadowp) {
  SimpleBaseString_sp nameKey = SimpleBaseString_O::make(std::string(symName));
  WITH_PACKAGE_READ_WRITE_LOCK(this);
  this->add_symbol_to_package_no_lock(nameKey, sym, exportp);
  if (shadowp) {
    this->_Shadowing->setf_gethash(sym, _lisp->_true());
  }
}

T_mv Package_O::intern(SimpleString_sp name) {
  bool ignore_lock = false;
 start: {
    WITH_PACKAGE_READ_WRITE_LOCK(this);
    // client_validate(ame);
    Symbol_mv values = this->findSymbol_SimpleString_no_lock(name);
    // client_validate(values->_Name);
    Symbol_sp sym = values;
    MultipleValues& mvn = core::lisp_multipleValues();
    Symbol_sp status = gc::As<Symbol_sp>(mvn.valueGet(1, values.number_of_values()));
    if (status.nilp()) {
      if (!ignore_lock && this->lockedP())
        goto package_lock_violation;
      sym = Symbol_O::create(name);
      client_validate(name);
      sym->makunbound();
      status = nil<Symbol_O>();
      sym->setPackage(this->sharedThis<Package_O>());
      LOG("Created symbol<{}>", _rep_(sym));
      this->add_symbol_to_package_no_lock(sym->symbolName(), sym, false);
    }
    if (this->actsLikeKeywordPackage()) {
      sym->setf_symbolValue(sym);
    } 

    //	trapSymbol(this,sym,name);
    LOG("Symbol[{}] interned as[{}]@{}", name, _rep_(sym), sym.get());
    return Values(sym, status);
  } // release lock

  package_lock_violation:
  CEpackage_lock_violation(this->asSmartPtr(), "interning ~s", 1, name);
  ignore_lock = true;
  goto start;
}

// This function is called by both unintern and shadowingImport.
// It removes a symbol from a package without doing any conflict checking.
// Make sure to hold the lock around this call.
bool Package_O::unintern_unsafe(Symbol_sp sym) {
  Symbol_sp status;
  SimpleString_sp nameKey = sym->_Name;
  // inherited symbols can't be on the shadow list, so this is safe.
  this->_Shadowing->remhash(sym);
  {
    Symbol_sp foundSym;
    Symbol_mv values = this->findSymbol_SimpleString_no_lock(nameKey);
    foundSym = values;
    MultipleValues& mvn = core::lisp_multipleValues();
    status = gc::As<Symbol_sp>(mvn.valueGet(1, values.number_of_values()));
    // If the symbol is not in the package there is nothing to unintern.
    if ((foundSym != sym) || (status.nilp()))
      return false;
  }
  if (status == kw::_sym_internal) {
    this->_InternalSymbols->remhash(nameKey);
    if (&*sym->getPackage() == this)
      sym->setPackage(nil<Package_O>());
    return true;
  } else if (status == kw::_sym_external) {
    this->_ExternalSymbols->remhash(nameKey);
    if (&*sym->getPackage() == this)
      sym->setPackage(nil<Package_O>());
    return true;
  }
  // symbol is accessible but inherited, i.e. not in the package.
  return false;
}

bool Package_O::unintern(Symbol_sp sym) {
  List_sp candidates = nil<List_V>(); // conflict resolution candidates
  bool ignore_lock = false;
 start: {
    WITH_PACKAGE_READ_WRITE_LOCK(this);
    // Don't allow uninterning from locked packages (e.g. CL)
    if (!ignore_lock && this->lockedP())
      goto package_lock_violation;
    SimpleString_sp nameKey = sym->_Name;
    if (this->_Shadowing->contains(sym)) {
      // Uninterning a shadowing symbol will cause a name conflict if this
      // packages uses multiple packages that export distinct symbols of the
      // same name.
      // We check for this before doing anything, because uninterning must be
      // all or nothing.
      // This is a list of symbols with the same name as the symbol
      // being uninterned that are exported by packages this package uses.
      MultipleValues& mvn = core::lisp_multipleValues();
      for (auto it : this->_UsingPackages) {
        Symbol_sp uf, status;
        {
          // FIXME: We don't have a lock on the other package!
          Symbol_mv values = it->findSymbol_SimpleString_no_lock(nameKey);
          uf = values;
          status = gc::As<Symbol_sp>(mvn.valueGet(1, values.number_of_values()));
        }
        // not sure if separate nilp is actually necessary
        if (status.nilp() ||
            (status != kw::_sym_external)
            // A symbol being accessible from different packages is ok.
            || (candidates.notnilp() && (gc::As_unsafe<Cons_sp>(candidates)->memberEq(uf)).notnilp()))
          continue;
        else
          candidates = Cons_O::create(uf, candidates);
      }
      // List complete - now, if there's more than one, we have conflict.
      if (cl__length(candidates) > 1)
        goto name_conflict;
    }
    return this->unintern_unsafe(sym);
  } // release lock
 name_conflict:
  // This function will resolve the conflict by shadowing-import-ing something.
  // It's defined later in CL because it uses the condition system heavily.
  eval::funcall(_sym_unintern_name_conflict, this->asSmartPtr(), sym, candidates);
  return true;
 package_lock_violation:
  CEpackage_lock_violation(this->asSmartPtr(), "uninterning ~s", 1, sym);
  ignore_lock = true;
  goto start;
}

bool Package_O::isExported(Symbol_sp sym) {
  WITH_PACKAGE_READ_LOCK(this);
  SimpleString_sp nameKey = sym->_Name;
  T_mv values = this->_ExternalSymbols->gethash(nameKey, nil<T_O>());
  MultipleValues& mvn = core::lisp_multipleValues();
  T_sp presentp = mvn.valueGet(1, values.number_of_values());
  LOG("isExported test of symbol[{}] isExported[{}]", sym->symbolNameAsString(), presentp.isTrue());
  return (presentp.isTrue());
}

void Package_O::import1(Symbol_sp symbolToImport) {
  Symbol_sp foundSymbol;
  bool ignore_lock = false;
 start: {
    WITH_PACKAGE_READ_WRITE_LOCK(this);
    if (!ignore_lock && this->lockedP()) {
      goto package_lock_violation;
    }
    SimpleString_sp nameKey = symbolToImport->_Name;
    Symbol_mv values = this->findSymbol_SimpleString_no_lock(nameKey);
    foundSymbol = values;
    MultipleValues& mvn = core::lisp_multipleValues();
    Symbol_sp status = gc::As<Symbol_sp>(mvn.valueGet(1, values.number_of_values()));
    if (status.nilp()) {
      // No conflict: add the symbol.
      this->add_symbol_to_package_no_lock(nameKey, symbolToImport, false);
      return;
    } else if (foundSymbol == symbolToImport) {
      // Symbol is already present: IMPORT does nothing.
      return;
    }
  }
  // Conflict - resolve w/o lock held as handlers can do crazy things
  eval::funcall(_sym_import_name_conflict, this->asSmartPtr(), foundSymbol, symbolToImport);
  return;
 package_lock_violation:
  CEpackage_lock_violation(this->asSmartPtr(), "importing ~s", 1,
                           symbolToImport);
  ignore_lock = true;
  goto start;
}

void Package_O::import(List_sp symbols) {
  for (auto cur : symbols) {
    Symbol_sp symbolToImport = gc::As<Symbol_sp>(oCar(cur));
    this->import1(symbolToImport);
  }
}

void Package_O::shadowingImport(List_sp symbols) {
  MultipleValues& mvn = core::lisp_multipleValues();
  WITH_PACKAGE_READ_WRITE_LOCK(this);
  for (auto cur : symbols) {
    Symbol_sp symbolToImport = gc::As<Symbol_sp>(oCar(cur));
    SimpleString_sp nameKey = symbolToImport->_Name;
    Symbol_mv values = this->findSymbol_SimpleString_no_lock(nameKey);
    Symbol_sp foundSymbol = values;
    Symbol_sp status = gc::As<Symbol_sp>(mvn.valueGet(1, values.number_of_values()));
    if (status == kw::_sym_internal || status == kw::_sym_external) {
      this->unintern_unsafe(foundSymbol);
    }
    this->add_symbol_to_package_no_lock(nameKey, symbolToImport, false);
    this->_Shadowing->setf_gethash(symbolToImport, _lisp->_true());
  }
}

List_sp Package_O::shadowingSymbols() const {
  WITH_PACKAGE_READ_LOCK(this);
  List_sp cur = nil<List_V>();
  this->_Shadowing->mapHash([&cur](T_sp symbol, T_sp dummy) { cur = Cons_O::create(symbol, cur); });
  return cur;
}

void Package_O::mapExternals(KeyValueMapper* mapper) {
  // I don't know what the caller will do with this so read/write lock
  WITH_PACKAGE_READ_WRITE_LOCK(this);
  this->_ExternalSymbols->lowLevelMapHash(mapper);
}

void Package_O::mapInternals(KeyValueMapper* mapper) {
  // I don't know what the caller will do with this so read/write lock
  WITH_PACKAGE_READ_WRITE_LOCK(this);
  this->_InternalSymbols->lowLevelMapHash(mapper);
}

void Package_O::dumpSymbols() {
  string all = this->allSymbols();
  printf("%s:%d Package %s\n", __FILE__, __LINE__, _rep_(this->_Name).c_str());
  printf("%s\n", all.c_str());
}

}; // namespace core
