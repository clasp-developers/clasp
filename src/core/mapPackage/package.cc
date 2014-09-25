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
#define	DEBUG_LEVEL_NONE

#include "core/common.h"
#include "corePackage.h"
#include "package.h"
#include "designators.h"
#include "str.h"
#include "archiveNode.h"
#include "archive.h"
#include "multipleValues.h"

// last include is wrappers.h
#include "wrappers.h"


namespace core
{


/*
  __BEGIN_DOC(candoScript.general.makePackage,makePackage)
  \scriptCmdRet{makePackage}{}{Text::packageName}

  Make the package.
  __END_DOC
*/

    
    
#define ARGS_af_makePackage "(package-name &key nicknames use)"
#define DECL_af_makePackage ""
#define DOCS_af_makePackage "make_package"
    T_sp af_makePackage(T_sp package_name_desig, Cons_sp nick_names, T_sp use_desig)
    {_G();
	Str_sp package_name = desig::stringDesignator(package_name_desig,_lisp);
	Cons_sp use_packages = desig::listOfPackageDesignators(use_desig,_lisp);
	list<string> lnn;
	for ( Cons_sp nc = nick_names; nc->notNil(); nc=nc->cdr() )
	{
	    Str_sp nickstr = desig::stringDesignator(nc->ocar(),_lisp);
	    lnn.push_front(nickstr->get());
	}
	list<Package_sp> lup;
	for ( Cons_sp uc = use_packages; uc->notNil(); uc=uc->cdr() ) lup.push_front(uc->car<Package_O>());
	return _lisp->makePackage(package_name->get(),lnn,lup);
    }





/*
  __BEGIN_DOC(candoScript.general.listAllPackages,listAllPackages)
  \scriptCmdRet{listAllPackages}{}{Text::packageName}

  Return a list of all packages.
  __END_DOC
*/

    
    
#define ARGS_af_list_all_packages ""
#define DECL_af_list_all_packages ""
#define DOCS_af_list_all_packages "list_all_packages"
    List_sp af_list_all_packages()
    {_G();
	Cons_sp packages = Cons_O::_nil;
	for ( Vector0<Package_O>::iterator mi = _lisp->packages().begin(); mi!= _lisp->packages().end(); mi++ )
	{
	    packages = Cons_O::create(*mi,packages,_lisp);
	}
	return packages;
    }




/*
  __BEGIN_DOC(candoScript.general.usePackage,usePackage)
  \scriptCmdRet{usePackage}{}{Symbol::packageName}

  Use the package. Return true if we used it.
  __END_DOC
*/

    
    
#define ARGS_af_use_package "(packages-to-use-desig &optional (package-desig *package*))"
#define DECL_af_use_package ""
#define DOCS_af_use_package "SeeCLHS use-package"
    T_sp af_use_package(T_sp packages_to_use_desig, T_sp package_desig)
    {_G();
	Cons_sp packages_to_use = desig::listOfPackageDesignators(packages_to_use_desig,_lisp);
	Package_sp package = desig::packageDesignator(package_desig);
	for ( Cons_sp cur = packages_to_use; cur->notNil(); cur=cur->cdr() )
	{
	    Package_sp package_to_use = cur->ocar()->as<Package_O>();
	    package->usePackage(package_to_use);
	}
	return _lisp->_true();
    }


#define ARGS_af_package_shadowing_symbols "(package_desig)"
#define DECL_af_package_shadowing_symbols ""
#define DOCS_af_package_shadowing_symbols "See CLHS package_shadowing_symbols"
    T_sp af_package_shadowing_symbols(T_sp package_desig)
    {_G();
	Package_sp package = desig::packageDesignator(package_desig);
	return package->shadowingSymbols();
    }

/*
  __BEGIN_DOC(candoScript.general.import,import)
  \scriptCmdRet{import}{}{symbols &optional package}

  Import the symbols into the (package) or the current package.
  __END_DOC
*/
#define ARGS_af_import "(symbols-desig &optional (package-desig *package*))"
#define DECL_af_import ""
#define DOCS_af_import "See CLHS: import"
    T_sp af_import(T_sp symbols_desig, T_sp package_desig)
    {_G();
	Cons_sp symbols = desig::listOfSymbols(symbols_desig,_lisp);
	Package_sp package = desig::packageDesignator(package_desig);
	package->import(symbols);
	return _lisp->_true();
    }



#define ARGS_af_shadow "(symbol-names-desig &optional (package_desig *package*))"
#define DECL_af_shadow ""
#define DOCS_af_shadow "See CLHS: shadow"
    T_sp af_shadow(Cons_sp symbol_names_desig, T_sp package_desig)
    {_G();
	Cons_sp symbolNames = desig::listOfStringDesignators(symbol_names_desig,_lisp);
	Package_sp package = desig::packageDesignator(package_desig);
	package->shadow(symbolNames);
	return _lisp->_true();
    }

    
    
#define ARGS_af_shadowing_import "(symbol-names-desig &optional (package-desig *package*))"
#define DECL_af_shadowing_import ""
#define DOCS_af_shadowing_import "See CLHS: shadowing-import"
    T_sp af_shadowing_import(Cons_sp symbol_names_desig, T_sp package_desig)
    {_G();
	Cons_sp symbolNames = desig::listOfSymbols(symbol_names_desig,_lisp);
	Package_sp package = desig::packageDesignator(package_desig);
	package->shadowingImport(symbolNames);
	return _lisp->_true();
    }


















    
    static    uint	static_gentemp_counter = 1;    
#define ARGS_af_gentemp "(&optional prefix (package *package*))"
#define DECL_af_gentemp ""
#define DOCS_af_gentemp "See CLHS gentemp"
    T_sp af_gentemp(Str_sp prefix, T_sp package_designator)
    {_G();
	stringstream ss;
	string spref = "T";
	Package_sp pkg = desig::packageDesignator(package_designator);
	if ( prefix->notNil() ) spref = prefix->get();
	for ( int i=0; i<1000; i++ )
	{
	    ss.str("");
	    ss << spref;
	    ss << static_gentemp_counter;
	    ++static_gentemp_counter;
	    MultipleValues_sp mv = pkg->findSymbol(ss.str());
	    if ( mv->osecond()->isNil() )
	    {
		mv = pkg->intern(ss.str());
		return mv->ofirst();
	    }
	}
	THROW(_lisp->error(BF("Could not find unique gentemp")));
    };


#define DOCS_af_package_use_list "package_use_list"
#define LOCK_af_package_use_list 0
#define ARGS_af_package_use_list "(package-designator)"
#define DECL_af_package_use_list ""    
    T_sp af_package_use_list(T_sp package_designator)
    {_G();
	Package_sp pkg = desig::packageDesignator(package_designator);
	return pkg->packageUseList();
    };


    void Package_O::exposeCando(Lisp_sp lisp)
    {_G();
	class_<Package_O>()
	    .def("allSymbols",&Package_O::allSymbols)
	    .def("packageName",&Package_O::packageName)
	    ;
	SYMBOL_EXPORT_SC_(package_use_list);
	Defun(package_use_list);
	SYMBOL_EXPORT_SC_(gentemp);
	Defun(gentemp);
	SYMBOL_EXPORT_SC_(makePackage);
	Defun(makePackage);
	SYMBOL_EXPORT_SC_(list_all_packages);
	Defun(list_all_packages);
	SYMBOL_EXPORT_SC_(use_package);
	Defun(use_package);
	SYMBOL_EXPORT_SC_(package_shadowing_symbols);
	Defun(package_shadowing_symbols);
	SYMBOL_EXPORT_SC_(import);
	Defun(import);
	SYMBOL_EXPORT_SC_(shadow);
	Defun(shadow);
	SYMBOL_EXPORT_SC_(shadowing_import);
	Defun(shadowing_import);
    }

    void Package_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef	USEBOOSTPYTHON //[
	PYTHON_CLASS(CorePkg,Package,"","",_lisp)
	    .def("allSymbols",&Package_O::allSymbols)
	    ;
#endif //]
    }


    Package_sp Package_O::create(Lisp_sp e, const string& name)
    {
	Package_sp p = Package_O::create(e);
	p->setName(name);
	return p;
    }


    T_sp 	Package_O::__init__(Function_sp exec, Cons_sp args, Environment_sp env, Lisp_sp lisp)
    {
	this->Base::__init__(exec,args,env,lisp);
	return T_O::_nil;
    }

    void	Package_O::initialize()
    {
	this->Base::initialize();
	this->_KeywordPackage = false;
	this->_AmpPackage = false;
    }

    string	Package_O::__repr__() const
    {
	stringstream ss;
	ss << "#<" << this->_Name << ">";
	return ss.str();
    }

    void	Package_O::archiveBase(ArchiveP node)
    {
	this->Base::archiveBase(node);
	node->attribute("name",this->_Name);
	node->archiveMap("internalSymbols",this->_InternalSymbols);
	node->archiveMap("externalSymbols",this->_ExternalSymbols);
	node->archiveSetIfDefined("usingPackages",this->_UsingPackages);
	node->archiveMapIfDefined("shadowingSymbols",this->_ShadowingSymbols);
    }


    string Package_O::allSymbols()
    {_G();
	stringstream ss;
	for ( StringMap<Symbol_O>::iterator mi=this->_InternalSymbols.begin(); mi!=this->_InternalSymbols.end(); mi++ )
	{
	    ss << "internal " << this->getName() << " --> " << mi->second->symbolName() << endl;
	}
	for ( StringMap<Symbol_O>::iterator mi=this->_ExternalSymbols.begin(); mi!=this->_ExternalSymbols.end(); mi++ )
	{
	    ss << "external " << this->getName() << " --> " << mi->second->symbolName() << endl;
	}
	return ss.str();
    }


    MultipleValues_sp Package_O::findSymbolDirectlyContained(const string& name)
    {_GF(BF("In package[%s] Looking up symbol for string[%s]") % this->__repr__() % name );
	// Look in ShadowingSymbols
	StringMap<Symbol_O>::iterator ei;
	// Look in the Symbol table
	ei = this->_ExternalSymbols.find(name);
	if ( ei!=this->_ExternalSymbols.end() )
	{
	    LOG(BF("Found it in the _ExternalsSymbols list - returning[%s]") % (ei->second->__repr__() ));
	    Symbol_sp status = _lisp->nil<Symbol_O>();
	    if ( IS_SID_DEFINED(_kw_external) ) status = _kw_external;
	    return MultipleValues_O::create(ei->second,status);
	}
	ei = this->_InternalSymbols.find(name);
	if ( ei!=this->_InternalSymbols.end() )
	{
	    LOG(BF("Found it in the _InternalSymbols list - returning[%s]") % (ei->second->__repr__() ));
	    Symbol_sp status = _lisp->nil<Symbol_O>();
	    if ( IS_SID_DEFINED(_kw_internal) ) status = _kw_internal;
	    return MultipleValues_O::create(ei->second,status);
	}
	return MultipleValues_O::_nil;
    }

    MultipleValues_sp Package_O::findSymbol(const string& name)
    {_GF(BF("In package[%s] Looking up symbol for string[%s]") % this->__repr__() % name );
	MultipleValues_sp mv = this->findSymbolDirectlyContained(name);
	if ( mv->notNil() ) return mv;
	{_BLOCK_TRACEF(BF("Looking in _UsingPackages"));
	    for ( Set<Package_O>::iterator it = this->_UsingPackages.begin();
		  it != this->_UsingPackages.end(); it++ )
	    {
		LOG(BF("Looking in package[%s]") % (*it)->__repr__() );
		MultipleValues_sp mv = (*it)->findSymbolDirectlyContained(name);
		if ( mv->isNil() ) continue;
		Symbol_sp uf = mv->get(0)->as<Symbol_O>();
		Symbol_sp status = mv->get(1)->as<Symbol_O>();
		if ( status->notNil() )
		{
		    if (status != _kw_external) continue;
		    LOG(BF("Found exported symbol[%s]") % uf->__repr__() );
		    return MultipleValues_O::create(uf,_kw_inherited);
		}
	    }
	}
	return MultipleValues_O::create(_lisp->nil<Symbol_O>(),_lisp->nil<Symbol_O>());
    }


    Cons_sp Package_O::packageUseList()
    {_OF();
	Cons_sp res = Cons_O::_nil;
	for ( Set<Package_O>::iterator si=this->_UsingPackages.begin();
	      si!=this->_UsingPackages.end(); si++ )
	{
	    res = Cons_O::create(*si,res,_lisp);
	}
	return res;
    }


    bool Package_O::areThereNameCollisions(Package_sp otherPackage)
    {
	for ( symbolIterator si=otherPackage->beginExternals(); si!=otherPackage->endExternals(); si++ )
	{
	    if ( this->findSymbol((si->second)->identifierName())->osecond()->notNil() ) return true; // previously ofirst()->notNil
	}
	return false;
    }


    bool Package_O::usePackage(Package_sp usePackage)
    {_OF();
	LOG(BF("In usePackage this[%s]  using package[%s]") % this->getName() % usePackage->getName() );
	if ( this->_UsingPackages.count(usePackage) > 0 )
	{
	    LOG(BF("You are already using that package"));
	    return true;
	}

	// 
	// Check for symbol conflicts
	//
	set<string> conflicts;
	for ( symbolIterator it = usePackage->beginExternals();
	      it != usePackage->endExternals(); it ++ )
	{
	    MultipleValues_sp mv = this->findSymbol((it->second)->identifierName());
	    Symbol_sp mine = mv->object()->as<Symbol_O>();
	    if ( mv->osecond()->notNil() && mine != it->second )
	    {
		LOG(BF("usePackage conflict - my symbol[%s] : usePackage symbol[%s]") % mine->__repr__() % (it->second)->__repr__());
		conflicts.insert((it->second)->identifierName());
	    }
	}
	if ( conflicts.size() > 0 )
	{
	    stringstream ss;
	    for ( set<string>::iterator si=conflicts.begin(); si!=conflicts.end(); si++ )
	    {
		ss << " " << *si;
	    }
	    THROW(_lisp->error(BF("Error: Name conflict when importing package[%s]"
				  " into package[%s] - conflicting symbols: %s")
			       % usePackage->getName()
			       % this->getName()
			       % (ss.str())));
	}
	this->_UsingPackages.insert(usePackage);
	return true;
    }



    void Package_O::_export(Cons_sp symbols)
    {_OF();
	for ( Cons_sp cur = symbols; cur->notNil(); cur = cur->cdr() )
	{
	    Symbol_sp sym = cur->car<Symbol_O>();
	    if ( sym->symbolName() == "" )
	    {
		THROW(_lisp->error(BF("Problem exporting symbol - it has no name")));
	    }
	    MultipleValues_sp mv = this->findSymbol(sym->symbolName());
	    Symbol_sp foundSym = mv->ofirst()->as<Symbol_O>();
	    Symbol_sp status = mv->get(1)->as<Symbol_O>();
	    LOG(BF("findSymbol status[%s]") % status->__repr__() );
	    if ( status->isNil() )
	    {
		this->_ExternalSymbols.set(sym->symbolName(),sym);
#if 0
	    } else if ( status == _kw_internal )
		LOG(BF("Symbol[%s] is being exported during startup - do this by hand") % sym->symbolName() );
		if ( this->_InternalSymbols.contains(sym->symbolName()) )
		{
		    this->_InternalSymbols.erase(sym->symbolName());
		    this->_ExternalSymbols.set(sym->symbolName(),sym);
		    LOG(BF("Symbol[%s] was moved from _InternalSymbols to _ExternalSymbols manually") % sym->symbolName() );
		} else
		{
		    LOG(BF("Symbol[%s] was not found in _InternalSymbols so nothing was done") % sym->symbolName() );
		}
#endif
	    } else if ( status == _kw_external )
	    {
		LOG(BF("Symbol[%s] is already in _ExternalSymbols - nothing to do") % sym->__repr__() );
		// do nothing its already external
	    } else if ( status == _kw_internal )
	    {
		LOG(BF("Moving symbol[%s] into _ExternalSymbols") % sym->__repr__() );
		this->_InternalSymbols.erase(sym->symbolName());
		this->_ExternalSymbols.set(sym->symbolName(),sym);
	    } else if ( status == _kw_inherited )
	    {
		LOG(BF("Symbol[%s] was inherited - importing it and then exporting it") % sym->__repr__() );
		this->import(Cons_O::create(sym,_lisp));
		this->_export(Cons_O::create(sym,_lisp));
	    }
	}
    }



    bool Package_O::shadow(Cons_sp symbolNames)
    {_OF();
	for ( Cons_sp cur = symbolNames; cur->notNil(); cur = cur->cdr() )
	{
	    Str_sp name = cur->car<Str_O>();
	    MultipleValues_sp mv = this->findSymbol(name->get());
	    Symbol_sp shadowSym = mv->first<Symbol_O>();
	    Symbol_sp status = mv->second<Symbol_O>();
	    if ( status->isNil() || (status != _kw_internal && status!=_kw_external) )
	    {
		shadowSym = Symbol_O::create(_lisp,name->get());
		shadowSym->makunbound();
		shadowSym->setPackage(this->sharedThis<Package_O>());
		LOG(BF("Created symbol<%s>") % shadowSym->__repr__() );
		this->_add_symbol_to_package(shadowSym);
	    }
	    this->_ShadowingSymbols.set(name->get(),shadowSym);
	}
	return true;
    }


    void Package_O::_add_symbol_to_package(Symbol_sp sym)
    {
	string name = sym->_Name;
	if ( this->isKeywordPackage() )
	{
	    this->_ExternalSymbols.set(name,sym);
	} else
	{
	    this->_InternalSymbols.set(name,sym);
	}
	if ( llvm_interface::addSymbol != NULL )
	{
	    llvm_interface::addSymbol(sym);
	}
    }




    MultipleValues_sp Package_O::intern(const string& name)
    {_OF();
	MultipleValues_sp vals = this->findSymbol(name);
	Symbol_sp sym = vals->ofirst()->as<Symbol_O>();
	Symbol_sp status = vals->get(1)->as<Symbol_O>();
	if ( status->isNil() )
	{
	    sym = Symbol_O::create(_lisp,name);
	    sym->makunbound();
	    status = _lisp->nil<Symbol_O>();
	    sym->setPackage(this->sharedThis<Package_O>());
	    LOG(BF("Created symbol<%s>") % sym->__repr__() );
	    this->_add_symbol_to_package(sym);
	}
	if ( this->isKeywordPackage() )
	{
	    sym->setf_symbolValue(sym);
	}
	LOG(BF("Symbol[%s] interned as[%s]@%p")% name % sym->__repr__() % sym.get() );
	return MultipleValues_O::create(sym,status);
    }



    bool Package_O::unintern(Symbol_sp sym)
    {_OF();
	LOG(BF("About to unintern symbol[%s]") % sym->__repr__() );
	// The following is not completely conformant with CLHS
	// unintern should throw an exception if removing a shadowing symbol
	// uncovers a name conflict of the symbol in two packages that are being used
	MultipleValues_sp mv = this->findSymbol(sym->symbolName());
	if ( mv->osecond()->notNil() )
	{
	    Symbol_sp sym = mv->ofirst()->as<Symbol_O>();
	    Symbol_sp status = mv->get(1)->as<Symbol_O>();
	    if ( this->_ShadowingSymbols.contains(sym->symbolName()) )
	    {
		this->_ShadowingSymbols.erase(sym->symbolName());
	    }
	    if ( status == _kw_internal )
	    {
		this->_InternalSymbols.erase(sym->symbolName());
		if (sym->getPackage().get() == this) sym->setPackage(_lisp->nil<Package_O>());
		return true;
	    } else if ( status == _kw_external)
	    {
		this->_ExternalSymbols.erase(sym->symbolName());
		if (sym->getPackage().get() == this) sym->setPackage(_lisp->nil<Package_O>());
		return true;
	    }
	}
	{_BLOCK_TRACEF(BF("Looking in _UsingPackages"));
	    set<Symbol_sp>	used_symbols;
	    for ( Set<Package_O>::iterator it = this->_UsingPackages.begin();
		  it != this->_UsingPackages.end(); it++ )
	    {
		MultipleValues_sp mv = (*it)->findSymbol(sym->symbolName());
		Symbol_sp uf = mv->get(0)->as<Symbol_O>();
		Symbol_sp status = mv->get(1)->as<Symbol_O>();
		if ( status->notNil() )
		{
		    if (status != _kw_external) continue;
		    used_symbols.insert(uf);
		}
	    }
	    if ( used_symbols.size() > 0 )
	    {
		stringstream ss;
		for ( set<Symbol_sp>::iterator sit=used_symbols.begin(); sit!=used_symbols.end(); sit++ )
		{
		    ss << " " << (*sit)->__repr__();
		}
		THROW(_lisp->error(BF("unintern symbol[%s] revealed name collision with used packages containing symbols: %s") % sym->__repr__() % ss.str()));
	    }
	}
	return false;
    }


    bool Package_O::isExported(Symbol_sp sym)
    {_OF();
	bool b = this->_ExternalSymbols.contains(sym->symbolName());
	LOG(BF("isExported test of symbol[%s] isExported[%d]") % sym->symbolName() % b);
	return b;
    }




    void Package_O::import(Cons_sp symbols)
    {_OF();
	for ( Cons_sp cur = symbols; cur->notNil(); cur = cur->cdr() )
	{
	    Symbol_sp symbolToImport = cur->ocar()->as<Symbol_O>();
	    MultipleValues_sp mv = this->findSymbol(symbolToImport->symbolName());
	    Symbol_sp foundSymbol = mv->first<Symbol_O>();
	    Symbol_sp status = mv->second<Symbol_O>();
	    if ( status == _kw_external || status == _kw_internal )
	    {
		// do nothing
	    } else if ( status == _kw_inherited || status->isNil() )
	    {
		this->_InternalSymbols.set(symbolToImport->symbolName(),symbolToImport);
	    } else
	    {
		THROW(_lisp->create<package_error_O>());
	    }
	}
    }


    void Package_O::shadowingImport(Cons_sp symbols)
    {_OF();
	for ( Cons_sp cur = symbols; cur->notNil(); cur = cur->cdr() )
	{
	    Symbol_sp symbolToImport = cur->ocar()->as<Symbol_O>();
	    MultipleValues_sp mv = this->findSymbol(symbolToImport->symbolName());
	    Symbol_sp foundSymbol = mv->first<Symbol_O>();
	    Symbol_sp status = mv->second<Symbol_O>();
	    if ( foundSymbol != symbolToImport )
	    {
		if ( status != _kw_inherited )
		{
		    this->unintern(foundSymbol);
		}
		this->_InternalSymbols.set(symbolToImport->symbolName(),symbolToImport);
	    }
	    this->_ShadowingSymbols.set(symbolToImport->symbolName(),symbolToImport);
	}
    }


    Cons_sp Package_O::shadowingSymbols() const
    {_OF();
	Cons_sp cur = Cons_O::_nil;
	for ( const_symbolIterator it=this->_ShadowingSymbols.begin();
	      it != this->_ShadowingSymbols.end(); it++ )
	{
	    cur = Cons_O::create(it->second,cur,_lisp);
	}
	return cur;
    }


    void Package_O::dumpSymbols()
    {_OF();
	string all = this->allSymbols();
	_lisp->print(BF("%s")%all);
    }



void Package_O::mapExternals(KeyValueMapper* mapper)
{_G();
    for ( symbolIterator si=this->beginExternals();
	  si!=this->endExternals(); si++ )
    {
	Str_sp key = Str_O::create(si->first);
	if ( !mapper->mapKeyValue(key,si->second) ) break;
    }
}

void Package_O::mapInternals(KeyValueMapper* mapper)
{_G();
    for ( symbolIterator si=this->beginInternals();
	  si!=this->endInternals(); si++ )
    {
	Str_sp key = Str_O::create(si->first);
	if ( !mapper->mapKeyValue(key,si->second) ) break;
    }
}


    EXPOSE_CLASS(core,Package_O);

};
