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
#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#ifdef DEBUG_CL_SYMBOLS
#include <clasp/core/allClSymbols.h>
#endif
#include <clasp/core/symbol.h>
#include <clasp/core/common.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/package.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/bignum.h>
#include <clasp/core/str.h>
#include <clasp/core/multipleValues.h>

// last include is wrappers.h
#include <clasp/core/wrappers.h>

namespace core {

CL_LAMBDA(package new-name &optional nick-names);
CL_DECLARE();
CL_DOCSTRING("renamePackage");
CL_DEFUN Package_sp cl__rename_package(T_sp pkg, T_sp newNameDesig, T_sp nickNameDesigs) {
  Package_sp package = coerce::packageDesignator(pkg);
  string newName = coerce::packageNameDesignator(newNameDesig);
  List_sp nickNames = coerce::listOfStringDesignators(nickNameDesigs);
  // Remove the old names from the Lisp system
  _lisp->unmapNameToPackage(package->getName());
  for (auto cur : package->getNicknames()) {
    _lisp->unmapNameToPackage(gc::As<Str_sp>(oCar(cur))->get());
  }
  // Set up the new names
  package->setName(newName);
  _lisp->mapNameToPackage(newName, package);
  for (auto cur : nickNames) {
    _lisp->mapNameToPackage(gc::As<Str_sp>(oCar(cur))->get(), package);
  };
  package->setNicknames(nickNames);
  return package;
};

CL_LAMBDA(pkg);
CL_DECLARE();
CL_DOCSTRING("packageNicknames");
CL_DEFUN T_sp cl__package_nicknames(T_sp pkg) {
  Package_sp package = coerce::packageDesignator(pkg);
  return package->getNicknames();
};

CL_LAMBDA(symbol &optional (package *package*));
CL_DECLARE();
CL_DOCSTRING("unintern");
CL_DEFUN bool cl__unintern(Symbol_sp sym, T_sp packageDesig) {
  Package_sp pkg = coerce::packageDesignator(packageDesig);
  return pkg->unintern(sym);
};

CL_LAMBDA(sym &optional (package *package*));
CL_DECLARE();
CL_DOCSTRING("findSymbol");
CL_DEFUN T_mv cl__find_symbol(const string &symbolname, T_sp packageDesig) {
  Package_sp package = coerce::packageDesignator(packageDesig);
  return package->findSymbol(symbolname);
};

CL_LAMBDA("package-name &key nicknames (use (list \"CL\"))");
CL_DECLARE();
CL_DOCSTRING("make_package");
CL_DEFUN T_mv cl__make_package(T_sp package_name_desig, List_sp nick_names, List_sp use_packages) {
  Str_sp package_name = coerce::stringDesignator(package_name_desig);
  list<string> lnn;
  for (auto nc : nick_names) {
    Str_sp nickstr = coerce::stringDesignator(oCar(nc));
    lnn.push_front(nickstr->get());
  }
  list<string> lup;
  for (auto uc : use_packages) {
    Package_sp pkg = coerce::packageDesignator(oCar(uc));
    lup.push_front(pkg->packageName());
  }
  return (Values(_lisp->makePackage(package_name->get(), lnn, lup)));
}

/*
  __BEGIN_DOC(candoScript.general.listAllPackages,listAllPackages)
  \scriptCmdRet{listAllPackages}{}{Text::packageName}

  Return a list of all packages.
  __END_DOC
*/

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("listAllPackages");
CL_DEFUN T_sp cl__list_all_packages() {
  List_sp packages = _Nil<List_V>();
  for (auto mi = _lisp->packages().begin(); mi != _lisp->packages().end(); mi++) {
    packages = Cons_O::create(*mi, packages);
  }
  return packages;
}

/*
  __BEGIN_DOC(candoScript.general.usePackage,usePackage)
  \scriptCmdRet{usePackage}{}{Symbol::packageName}

  Use the package. Return true if we used it.
  __END_DOC
*/

CL_LAMBDA(packages-to-use-desig &optional (package-desig *package*));
CL_DECLARE();
CL_DOCSTRING("SeeCLHS use-package");
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
CL_DOCSTRING("SeeCLHS unuse-package");
CL_DEFUN T_sp cl__unuse_package(T_sp packages_to_unuse_desig, T_sp package_desig) {
  List_sp packages_to_unuse = coerce::listOfPackageDesignators(packages_to_unuse_desig);
  Package_sp package = coerce::packageDesignator(package_desig);
  for (auto cur : packages_to_unuse) {
    Package_sp package_to_unuse = gc::As<Package_sp>(oCar(cur));
    package->unusePackage(package_to_unuse);
  }
  return _lisp->_true();
}

CL_LAMBDA(packages-to-unuse-desig &optional (package-desig *package*));
CL_DECLARE();
CL_DOCSTRING("SeeCLHS unuse-package");
CL_DEFUN T_sp cl__delete_package(T_sp pobj) {
  T_sp hash, l;
  cl_index i;
  Package_sp pkg = coerce::packageDesignator(pobj);
  if (pkg == _lisp->commonLispPackage() || pkg == _lisp->keywordPackage()) {
    FEpackage_error("Cannot delete the package ~S", pkg, 0);
  }

  /* 2) Now remove the package from the other packages that use it
	 *    and empty the package.
	 */
  if (pkg->packageName() == "") {
    return _Nil<T_O>();
  }
  for (auto pi : pkg->_UsingPackages) {
    if (pi.notnilp())
      cl__unuse_package(pi, pkg);
  }
  for (auto pi : pkg->_PackagesUsedBy) {
    if (pi.notnilp())
      cl__unuse_package(pkg, pi);
  }

  IMPLEMENT_MEF(BF("Finish implementing delete-package"));
#if 0

  ECL_WITH_GLOBAL_ENV_WRLOCK_BEGIN(ecl_process_env()) {
    for (hash = p->pack.internal, i = 0; i < hash->hash.size; i++)
      if (hash->hash.data[i].key != OBJNULL) {
        T_sp s = hash->hash.data[i].value;
        symbol_remove_package(s, p);
      }
    cl__clrhash(p->pack.internal);
    for (hash = p->pack.external, i = 0; i < hash->hash.size; i++)
      if (hash->hash.data[i].key != OBJNULL) {
        T_sp s = hash->hash.data[i].value;
        symbol_remove_package(s, p);
      }
    cl__clrhash(p->pack.external);
    p->pack.shadowings = ECL_NIL;
    p->pack.name = ECL_NIL;
                /* 2) Only at the end, remove the package from the list of packages. */
    cl_core.packages = ecl_remove_eq(p, cl_core.packages);
  } ECL_WITH_GLOBAL_ENV_WRLOCK_END;
  @(return ECL_T);
#endif
}

CL_LAMBDA(package_desig);
CL_DECLARE();
CL_DOCSTRING("See CLHS package_shadowing_symbols");
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
CL_DOCSTRING("See CLHS: import");
CL_DEFUN T_mv cl__import(T_sp symbols_desig, T_sp package_desig) {
  List_sp symbols = coerce::listOfSymbols(symbols_desig);
  Package_sp package = coerce::packageDesignator(package_desig);
  package->import(symbols);
  return (Values(_lisp->_true()));
}

CL_LAMBDA(symbol-names-desig &optional (package_desig *package*));
CL_DECLARE();
CL_DOCSTRING("See CLHS: shadow");
CL_DEFUN T_mv cl__shadow(List_sp symbol_names_desig, T_sp package_desig) {
  List_sp symbolNames = coerce::listOfStringDesignators(symbol_names_desig);
  Package_sp package = coerce::packageDesignator(package_desig);
  package->shadow(symbolNames);
  return (Values(_lisp->_true()));
}

CL_LAMBDA(symbol-names-desig &optional (package-desig *package*));
CL_DECLARE();
CL_DOCSTRING("See CLHS: shadowing-import");
CL_DEFUN T_mv cl__shadowing_import(T_sp symbol_names_desig, T_sp package_desig) {
  List_sp symbolNames = coerce::listOfSymbols(symbol_names_desig);
  Package_sp package = coerce::packageDesignator(package_desig);
  package->shadowingImport(symbolNames);
  return (Values(_lisp->_true()));
}

static uint static_gentemp_counter = 1;
CL_LAMBDA(&optional prefix (package *package*));
CL_DECLARE();
CL_DOCSTRING("See CLHS gentemp");
CL_DEFUN T_mv cl__gentemp(T_sp prefix, T_sp package_designator) {
  stringstream ss;
  string spref = "T";
  Package_sp pkg = coerce::packageDesignator(package_designator);
  if (prefix.notnilp())
    spref = gc::As<Str_sp>(prefix)->get();
  T_sp retval;
  for (int i = 0; i < 1000; i++) {
    ss.str("");
    ss << spref;
    ss << static_gentemp_counter;
    ++static_gentemp_counter;
    {
      MULTIPLE_VALUES_CONTEXT();
      T_mv mv = pkg->findSymbol(ss.str());
      if (gc::As<T_sp>(mv.valueGet(1)).nilp()) {
        {
          MULTIPLE_VALUES_CONTEXT();
          retval = pkg->intern(ss.str());
          goto DONE;
        }
      }
    }
  }
  SIMPLE_ERROR(BF("Could not find unique gentemp"));
DONE:
  return (Values(retval));
};

CL_LAMBDA(package-designator);
CL_DECLARE();
CL_DOCSTRING("package_use_list");
CL_DEFUN T_sp cl__package_use_list(T_sp package_designator) {
  Package_sp pkg = coerce::packageDesignator(package_designator);
  return pkg->packageUseList();
};

CL_LAMBDA(pkg);
CL_DECLARE();
CL_DOCSTRING("packageUsedByList");
CL_DEFUN List_sp cl__package_used_by_list(T_sp pkgDesig) {
  Package_sp pkg = coerce::packageDesignator(pkgDesig);
  return pkg->packageUsedByList();
};

CL_LAMBDA(pkg);
CL_DECLARE();
CL_DOCSTRING("packageName");
CL_DEFUN T_sp cl__package_name(T_sp pkgDesig) {
  Package_sp pkg = coerce::packageDesignator(pkgDesig);
  string name = pkg->packageName();
  if (name == "") {
    return _Nil<T_O>();
  }
  return Str_O::create(name);
};

void Package_O::exposeCando(Lisp_sp lisp) {
  class_<Package_O>()
      //	    .def("allSymbols",&Package_O::allSymbols)
      .def("core:PackageHashTables", &Package_O::hashTables);
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
}

void Package_O::exposePython(Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON //[
  PYTHON_CLASS(CorePkg, Package, "", "", _lisp)
      //	    .def("allSymbols",&Package_O::allSymbols)
      ;
#endif //]
}

Package_sp Package_O::create(const string &name) {
  Package_sp p = Package_O::create();
  p->setName(name);
  return p;
}

void Package_O::initialize() {
  this->Base::initialize();
  this->_InternalSymbols = HashTableEqual_O::create_default();
  this->_ExternalSymbols = HashTableEqual_O::create_default();
  this->_Shadowing = HashTableEq_O::create_default();
  this->_KeywordPackage = false;
  this->_AmpPackage = false;
}

CL_LISPIFY_NAME("core:PackageHashTables");
CL_DEFMETHOD T_mv Package_O::hashTables() const {
  List_sp useList = _Nil<List_V>();
  for (auto ci = this->_UsingPackages.begin();
       ci != this->_UsingPackages.end(); ci++) {
    useList = Cons_O::create(*ci, useList);
  }
  return (Values(this->_ExternalSymbols, this->_InternalSymbols, useList));
}

string Package_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_Name.asStdString() << ">";
  return ss.str();
}

#if defined(XML_ARCHIVE)
void Package_O::archiveBase(ArchiveP node) {
  IMPLEMENT_MEF(BF("Handle archiving the package hash-tables"));
#if 0
	this->Base::archiveBase(node);
	node->attribute("name",this->_Name);
	node->archiveMap("internalSymbols",this->_InternalSymbols);
	node->archiveMap("externalSymbols",this->_ExternalSymbols);
	node->archiveSetIfDefined("usingPackages",this->_UsingPackages);
	node->archiveMapIfDefined("shadowingSymbols",this->_ShadowingSymbols);
#endif
}
#endif // defined(XML_ARCHIVE)

class PackageMapper : public KeyValueMapper {
public:
  stringstream *ssP;
  string type;
  PackageMapper(const string &t, stringstream *sstr) : ssP(sstr), type(t){};
  virtual bool mapKeyValue(T_sp key, T_sp value) {
    (*(this->ssP)) << type << " " << _rep_(value) << std::endl;
    return true;
  }
};

string Package_O::allSymbols() {
  stringstream ss;
  PackageMapper internals("internal", &ss);
  this->_InternalSymbols->lowLevelMapHash(&internals);
  PackageMapper externals("external", &ss);
  this->_ExternalSymbols->lowLevelMapHash(&externals);
  return ss.str();
}

Symbol_mv Package_O::_findSymbol(Str_sp nameKey) const {
  T_mv ei = this->_ExternalSymbols->gethash(nameKey, _Nil<T_O>());
  Symbol_sp val = gc::As<Symbol_sp>(ei);
  bool foundp = ei.second().isTrue();
  if (foundp) {
    LOG(BF("Found it in the _ExternalsSymbols list - returning[%s]") % (_rep_(val)));
    return (Values(val, kw::_sym_external));
  }
  // There is no need to look further if this is the keyword package
  if (this->isKeywordPackage())
    return Values(_Nil<T_O>(), _Nil<T_O>());
  T_mv ej = this->_InternalSymbols->gethash(nameKey, _Nil<T_O>());
  val = gc::As<Symbol_sp>(ej);
  foundp = ej.second().isTrue();
  if (foundp) {
    LOG(BF("Found it in the _InternalSymbols list - returning[%s]") % (_rep_(first)));
    return (Values(val, kw::_sym_internal));
  }
  {
    _BLOCK_TRACEF(BF("Looking in _UsingPackages"));
    for (auto it = this->_UsingPackages.begin();
         it != this->_UsingPackages.end(); it++) {
      Package_sp upkg = *it;
      LOG(BF("Looking in package[%s]") % _rep_(upkg));

      T_mv eu = upkg->_ExternalSymbols->gethash(nameKey, _Nil<T_O>());
      val = gc::As<Symbol_sp>(eu);
      foundp = ei.second().isTrue();
      if (foundp) {
        LOG(BF("Found it in the _ExternalsSymbols list - returning[%s]") % (_rep_(val)));
        return (Values(val, kw::_sym_inherited));
      }
    }
  }
  return (Values(_Nil<Symbol_O>(), _Nil<Symbol_O>()));
}

Symbol_mv Package_O::findSymbol(const string &name) const {
  Str_sp sname = Str_O::create(name);
  return this->_findSymbol(sname);
}

List_sp Package_O::packageUseList() {
  _OF();
  List_sp res = _Nil<List_V>();
  for (auto si = this->_UsingPackages.begin();
       si != this->_UsingPackages.end(); si++) {
    res = Cons_O::create(*si, res);
  }
  return res;
}

List_sp Package_O::packageUsedByList() {
  _OF();
  List_sp res = _Nil<List_V>();
  for (auto si = this->_PackagesUsedBy.begin();
       si != this->_PackagesUsedBy.end(); si++) {
    res = Cons_O::create(*si, res);
  }
  return res;
}

T_mv Package_O::packageHashTables() const {
  List_sp usingPackages = _Nil<List_V>();
  for (auto si = this->_UsingPackages.begin();
       si != this->_UsingPackages.end(); si++) {
    usingPackages = Cons_O::create(*si, usingPackages);
  }
  return (Values(this->_ExternalSymbols,
                 this->_InternalSymbols,
                 usingPackages));
}

bool Package_O::usingPackageP(Package_sp usePackage) const {
  for (auto it = this->_UsingPackages.begin(); it != this->_UsingPackages.end(); ++it) {
    if ((*it) == usePackage)
      return true;
  }
  return false;
}

bool Package_O::usePackage(Package_sp usePackage) {
  _OF();
  LOG(BF("In usePackage this[%s]  using package[%s]") % this->getName() % usePackage->getName());
  if (this->usingPackageP(usePackage)) {
    LOG(BF("You are already using that package"));
    return true;
  }
  //
  // Check for symbol conflicts
  //
  FindConflicts findConflicts(this->asSmartPtr());
  this->_ExternalSymbols->lowLevelMapHash(&findConflicts);
  if (findConflicts._conflicts.size() > 0) {
    stringstream ss;
    for (set<string>::iterator si = findConflicts._conflicts.begin();
         si != findConflicts._conflicts.end(); si++) {
      ss << " " << *si;
    }
    SIMPLE_ERROR(BF("Error: Name conflict when importing package[%s]"
                    " into package[%s] - conflicting symbols: %s") %
                 usePackage->getName() % this->getName() % (ss.str()));
  }
  this->_UsingPackages.push_back(usePackage);
  Package_sp me(this);
  usePackage->_PackagesUsedBy.push_back(me);
  return true;
}

bool Package_O::unusePackage(Package_sp usePackage) {
  _OF();
  Package_sp me(this);
  for (auto it = this->_UsingPackages.begin();
       it != this->_UsingPackages.end(); ++it) {
    if ((*it) == usePackage) {
      this->_UsingPackages.erase(it);
      for (auto jt = usePackage->_PackagesUsedBy.begin();
           jt != usePackage->_PackagesUsedBy.end(); ++jt) {
        if (*jt == me) {
          usePackage->_PackagesUsedBy.erase(jt);
          return true;
        }
      }
      SIMPLE_ERROR(BF("The unusePackage argument %s is not used by my package %s") % usePackage->getName() % this->getName());
    }
  }
  return true;
}

bool FindConflicts::mapKeyValue(T_sp key, T_sp value) {
  Str_sp nameKey = gc::As<Str_sp>(key);
  Symbol_sp svalue = gc::As<Symbol_sp>(value);

  Symbol_mv values = this->_me->_findSymbol(nameKey);
  Symbol_sp mine = values;
  T_sp foundp = values.second();
  if (foundp.notnilp() && mine != svalue) {
    LOG(BF("usePackage conflict - my symbol[%s] : usePackage symbol[%s]") % _rep_(mine) % _rep_(svalue));
    this->_conflicts.insert(svalue->symbolNameAsString());
  }
  return true;
}

/*! Return a NULL package if there is no conflict */
Package_sp Package_O::export_conflict_or_NULL(Str_sp nameKey, Symbol_sp sym) {
  for (auto use_pkg : this->_PackagesUsedBy) {
    Symbol_mv x = use_pkg->_findSymbol(nameKey);
    Symbol_sp xsym = x;
    Symbol_sp status = gc::As<Symbol_sp>(x.second());
    if (status.notnilp() && sym != xsym &&
        !use_pkg->_Shadowing->contains(xsym)) {
      return use_pkg;
    }
  }
  Package_sp noConflict;
  return noConflict;
}

typedef enum { no_problem,
               no_problem_already_exported,
               not_accessible_in_this_package,
               already_symbol_with_same_name_in_this_package,
               name_conflict_in_other_package } Export_errors;
void Package_O::_export2(Symbol_sp sym) {
  Str_sp nameKey = sym->_Name;
  Package_sp error_pkg;
  Export_errors error;
  { // TODO: threading   ECL DOES A GLOBAL WRITE LOCK HERE
    T_mv values = this->_findSymbol(nameKey);
    Symbol_sp foundSym = gc::As<Symbol_sp>(values);
    Symbol_sp status = gc::As<Symbol_sp>(values.second());
    if (status.nilp()) {
      error = not_accessible_in_this_package;
    } else if (foundSym != sym) {
      error = already_symbol_with_same_name_in_this_package;
    } else if (status == kw::_sym_external) {
      error = no_problem_already_exported;
    } else if (Package_sp pkg_with_conflict = this->export_conflict_or_NULL(nameKey, sym)) {
      error = name_conflict_in_other_package;
      error_pkg = pkg_with_conflict;
    } else {
      if (status == kw::_sym_internal) {
        this->_InternalSymbols->remhash(nameKey);
      }
      this->_ExternalSymbols->hash_table_setf_gethash(nameKey, sym);
      error = no_problem;
    }
  } // TO HERE
  if (error == not_accessible_in_this_package) {
    CEpackage_error("The symbol ~S is not accessible from ~S "
                    "and cannot be exported.",
                    "Import the symbol in the package and proceed.",
                    this->asSmartPtr(), 2, sym.raw_(), this->asSmartPtr().raw_());
  } else if (error == already_symbol_with_same_name_in_this_package) {
    FEpackage_error("Cannot export the symbol ~S from ~S,~%"
                    "because there is already a symbol with the same name~%"
                    "in the package.",
                    this->asSmartPtr(), 2, sym.raw_(), this->asSmartPtr().raw_());
  } else if (error == name_conflict_in_other_package) {
    FEpackage_error("Cannot export the symbol ~S~%"
                    "from ~S,~%"
                    "because it will cause a name conflict~%"
                    "in ~S.",
                    this->asSmartPtr(), 3, sym.raw_(), this->asSmartPtr().raw_(), error_pkg.raw_());
  }
}

bool Package_O::shadow(Str_sp symbolName) {
  Symbol_sp shadowSym, status;
  Symbol_mv values = this->findSymbol(symbolName->get());
  shadowSym = values;
  status = gc::As<Symbol_sp>(values.valueGet(1));
  if (status.nilp() || (status != kw::_sym_internal && status != kw::_sym_external)) {
    shadowSym = Symbol_O::create(symbolName->get());
    shadowSym->makunbound();
    shadowSym->setPackage(this->sharedThis<Package_O>());
    LOG(BF("Created symbol<%s>") % _rep_(shadowSym));
    this->add_symbol_to_package(shadowSym->symbolName()->get().c_str(), shadowSym);
  }
  this->_Shadowing->setf_gethash(shadowSym, _lisp->_true());
  return true;
}

bool Package_O::shadow(List_sp symbolNames) {
  _OF();
  for (auto cur : symbolNames) {
    Str_sp name = gc::As<Str_sp>(oCar(cur));
    this->shadow(name);
  }
  return true;
}

void trapSymbol(Package_O *pkg, Symbol_sp sym, const string &name) {
#if 0
        // Dump every symbol
        printf("INTERNING symbol[%s]@%p\n", name.c_str(), sym.raw_() );
#endif
#if 0
	if ( name == "COERCE-NAME") // == "+HASH-TABLE-ENTRY-VALUE-HAS-BEEN-DELETED+")
	{
	    printf("%s:%d - Package_O::intern of %s@%p in package %s\n",
		   __FILE__, __LINE__, name.c_str(), sym.raw_(), pkg->getName().c_str() );
	}
#endif
}

void Package_O::add_symbol_to_package(const char *symName, Symbol_sp sym, bool exportp) {
  //trapSymbol(this,sym,symName);
  if (_lisp->_TrapIntern) {
    if (strcmp(this->_Name.c_str(), _lisp->_TrapInternPackage.c_str()) == 0) {
      if (strcmp(symName, _lisp->_TrapInternName.c_str()) == 0) {
        printf("%s:%d TRAPPED INTERN of symbol %s@%p in package %s\n", __FILE__, __LINE__, symName, sym.raw_(), this->_Name.c_str() );
      }
    }
  }
#if 0
  if ( strcmp(symName,"YES-OR-NO-P") == 0 ) {
    printf("%s:%d Interning YES-OR-NO-P\n", __FILE__, __LINE__ );
  }
#endif
//  printf("%s:%d Interning symbol %s@%p into %s exportp: %d\n", __FILE__, __LINE__, _rep_(sym).c_str(), sym.raw_(), this->_Name.c_str(), exportp);
#if 0 // DEBUG_CL_SYMBOLS
  if (!exportp && sym.notnilp() && this == &(*(_lisp->commonLispPackage()))) {
    printf("%s:%d Interning an internal symbol %s within COMMON-LISP\n", __FILE__, __LINE__, symName );
  }
#endif
  Str_sp nameKey = Str_O::create(symName);
  if (this->isKeywordPackage() || this->actsLikeKeywordPackage() || exportp) {
#if 0
    if (sym.notnilp() && this == &(*(_lisp->commonLispPackage()))) {
      throwIfNotValidClSymbol(sym->symbolName()->get());
    }
#endif
    this->_ExternalSymbols->hash_table_setf_gethash(nameKey, sym);
  } else {
    this->_InternalSymbols->hash_table_setf_gethash(nameKey, sym);
  }
  if (llvm_interface::addSymbol != NULL) {
    DEPRECIATED();
    llvm_interface::addSymbol(sym);
  }
}




void Package_O::bootstrap_add_symbol_to_package(const char *symName, Symbol_sp sym, bool exportp, bool shadowp) {
  this->add_symbol_to_package(symName,sym,exportp);
  if ( shadowp ) {
    this->_Shadowing->setf_gethash(sym,_lisp->_true());
  }
}

T_mv Package_O::intern(const string &name) {
  Symbol_mv values = this->findSymbol(name);
  Symbol_sp sym = values;
  Symbol_sp status = gc::As<Symbol_sp>(values.valueGet(1));
  if (status.nilp()) {
    sym = Symbol_O::create(name);
    sym->makunbound();
    status = _Nil<Symbol_O>();
    sym->setPackage(this->sharedThis<Package_O>());
    LOG(BF("Created symbol<%s>") % _rep_(sym));
    this->add_symbol_to_package(sym->symbolName()->get().c_str(), sym);
  }
  if (this->actsLikeKeywordPackage()) {
    sym->setf_symbolValue(sym);
  }

  //	trapSymbol(this,sym,name);
  LOG(BF("Symbol[%s] interned as[%s]@%p") % name % _rep_(sym) % sym.get());
  return (Values(sym, status));
}

bool Package_O::unintern(Symbol_sp sym) {
  // The following is not completely conformant with CLHS
  // unintern should throw an exception if removing a shadowing symbol
  // uncovers a name conflict of the symbol in two packages that are being used
  Str_sp nameKey = sym->_Name;
  {
    Symbol_sp sym, status;
    {
      Symbol_mv values = this->_findSymbol(nameKey);
      sym = values;
      status = gc::As<Symbol_sp>(values.valueGet(1));
    }
    if (status.notnilp()) {
      if (this->_Shadowing->contains(sym)) {
        this->_Shadowing->remhash(sym);
      }
      if (status == kw::_sym_internal) {
        this->_InternalSymbols->remhash(nameKey);
        if (sym->getPackage().get() == this)
          sym->setPackage(_Nil<Package_O>());
        return true;
      } else if (status == kw::_sym_external) {
        this->_ExternalSymbols->remhash(nameKey);
        if (sym->getPackage().get() == this)
          sym->setPackage(_Nil<Package_O>());
        return true;
      }
    }
  }
  {
    _BLOCK_TRACEF(BF("Looking in _UsingPackages"));
    HashTableEq_sp used_symbols(HashTableEq_O::create());
    for (auto it = this->_UsingPackages.begin();
         it != this->_UsingPackages.end(); it++) {
      Symbol_sp uf, status;
      {
        MULTIPLE_VALUES_CONTEXT();
        Symbol_mv values = (*it)->_findSymbol(nameKey);
        uf = values;
        status = gc::As<Symbol_sp>(values.valueGet(1));
      }
      if (status.notnilp()) {
        if (status != kw::_sym_external)
          continue;
        used_symbols->insert(uf);
      }
    }
    if (used_symbols->size() > 0) {
      SIMPLE_ERROR(BF("unintern symbol[%s] revealed name collision with used packages containing symbols: %s") % _rep_(sym) % _rep_(used_symbols->keysAsCons()));
    }
  }
  return false;
}

bool Package_O::isExported(Symbol_sp sym) {
  Str_sp nameKey = sym->_Name;
  T_mv values = this->_ExternalSymbols->gethash(nameKey, _Nil<T_O>());
  T_sp presentp = values.valueGet(1);
  LOG(BF("isExported test of symbol[%s] isExported[%d]") % sym->symbolNameAsString() % presentp.isTrue());
  return (presentp.isTrue());
}

void Package_O::import(List_sp symbols) {
  _OF();
  for (auto cur : symbols) {
    Symbol_sp symbolToImport = gc::As<Symbol_sp>(oCar(cur));
    Str_sp nameKey = symbolToImport->_Name;
    Symbol_mv values = this->_findSymbol(nameKey);
    Symbol_sp foundSymbol = values;
    Symbol_sp status = gc::As<Symbol_sp>(values.valueGet(1));
    if (status == kw::_sym_external || status == kw::_sym_internal) {
      // do nothing
    } else if (status == kw::_sym_inherited || status.nilp()) {
      this->_InternalSymbols->hash_table_setf_gethash(nameKey, symbolToImport);
    } else {
      PACKAGE_ERROR(this->sharedThis<Package_O>());
    }
  }
}

void Package_O::shadowingImport(List_sp symbols) {
  for (auto cur : symbols) {
    Symbol_sp symbolToImport = gc::As<Symbol_sp>(oCar(cur));
    Str_sp nameKey = symbolToImport->_Name;
    Symbol_mv values = this->_findSymbol(nameKey);
    Symbol_sp foundSymbol = values;
    Symbol_sp status = gc::As<Symbol_sp>(values.valueGet(1));
    if (status == kw::_sym_internal || status == kw::_sym_external) {
      this->unintern(foundSymbol);
    }
    this->_InternalSymbols->hash_table_setf_gethash(nameKey, symbolToImport);
    this->_Shadowing->setf_gethash(symbolToImport, _lisp->_true());
  }
}

List_sp Package_O::shadowingSymbols() const {
  _OF();
  List_sp cur = _Nil<List_V>();
  this->_Shadowing->mapHash([&cur](T_sp symbol, T_sp dummy) {
	    cur = Cons_O::create(symbol,cur);
  });
  return cur;
}

void Package_O::mapExternals(KeyValueMapper *mapper) {
  this->_ExternalSymbols->lowLevelMapHash(mapper);
}

void Package_O::mapInternals(KeyValueMapper *mapper) {
  this->_InternalSymbols->lowLevelMapHash(mapper);
}

void Package_O::dumpSymbols() {
  _OF();
  string all = this->allSymbols();
  printf("%s:%d Package %s\n", __FILE__, __LINE__, this->_Name.c_str());
  printf("%s\n", all.c_str());
}

EXPOSE_CLASS(core, Package_O);
};
