
#include "foundation.h"
#include "lisp.h"
#include "extensionPackage.fwd.h"
#include "symbolTable.h"
#include "bootStrapCoreSymbolMap.h"

namespace core
{

    string BootStrapCoreSymbolMap::fullSymbolName(string const& packageName, string const& symbolName)
    {
	string fullName = packageName+"::"+symbolName;
	return((fullName));
    }

    BootStrapCoreSymbolMap::BootStrapCoreSymbolMap()
    {
	this->attachToGCRoot();
//#define LOOKUP_SYMBOL(pkgName,symName) bootStrapSymbolMap.lookupSymbol(pkgName,symName)
#define ClPkg_SYMBOLS
#define DO_SYMBOL(cname,rsid,pkgName,symName,exportp) cl::cname = this->allocate_unique_symbol(pkgName,symName,exportp);
	#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef ClPkg_SYMBOLS
#define CorePkg_SYMBOLS
#define DO_SYMBOL(cname,rsid,pkgName,symName,exportp) core::cname = this->allocate_unique_symbol(pkgName,symName,exportp);
	#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef CorePkg_SYMBOLS

#define KeywordPkg_SYMBOLS
#define DO_SYMBOL(cname,rsid,pkgName,symName,exportp) kw::cname = this->allocate_unique_symbol(pkgName,symName,exportp);
	#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef KeywordPkg_SYMBOLS

#define ExtPkg_SYMBOLS
#define DO_SYMBOL(cname,rsid,pkgName,symName,exportp) ext::cname = this->allocate_unique_symbol(pkgName,symName,exportp);
	#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef ExtPkg_SYMBOLS

#define ClosPkg_SYMBOLS
#define DO_SYMBOL(cname,rsid,pkgName,symName,exportp) clos::cname = this->allocate_unique_symbol(pkgName,symName,exportp);
	#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef ClosPkg_SYMBOLS

#define GrayPkg_SYMBOLS
#define DO_SYMBOL(cname,rsid,pkgName,symName,exportp) gray::cname = this->allocate_unique_symbol(pkgName,symName,exportp);
	#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef GrayPkg_SYMBOLS


#define CompPkg_SYMBOLS
#define DO_SYMBOL(cname,rsid,pkgName,symName,exportp) comp::cname = this->allocate_unique_symbol(pkgName,symName,exportp);
	#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef CompPkg_SYMBOLS

    };

    Symbol_sp BootStrapCoreSymbolMap::allocate_unique_symbol(string const& pkgName, string const& symbolName,bool exportp)
    {
	string name = BootStrapCoreSymbolMap::fullSymbolName(pkgName,symbolName);
	map<string,SymbolStorage>::iterator it = this->_SymbolNamesToSymbol.find(name);
	if ( it != this->_SymbolNamesToSymbol.end() )
	{
	    return((it->second._Symbol));
	}
	Symbol_sp sym = Symbol_O::create(symbolName);
	SymbolStorage store(pkgName,symbolName,sym,exportp);
	this->_SymbolNamesToSymbol[name] = store;
	return((sym));
    }

    Symbol_sp BootStrapCoreSymbolMap::lookupSymbol(string const& packageName, string const& rawSymbolName) const
    {
	string symbolName = rawSymbolName;
	string fullName = BootStrapCoreSymbolMap::fullSymbolName(packageName,symbolName);
	map<string,SymbolStorage>::const_iterator it = this->_SymbolNamesToSymbol.find(fullName);
	if ( it == this->_SymbolNamesToSymbol.end() )
	{
	    THROW_HARD_ERROR(BF("In BootStrapCoreSymbolMap::lookupSymbol Unknown symbolName[%s]") % fullName );
	}
	return((it->second._Symbol));
    }



    void BootStrapCoreSymbolMap::finish_setup_of_symbols()
    {_G();
	for ( map<string,SymbolStorage>::const_iterator it=this->_SymbolNamesToSymbol.begin();
	      it!=this->_SymbolNamesToSymbol.end(); it++ )
	{
	    Package_sp pkg = _lisp->findPackage(it->second._PackageName);
	    it->second._Symbol->finish_setup(pkg,it->second._Export);
	}
    }

    void BootStrapCoreSymbolMap::dump()
    {
	for ( map<string,SymbolStorage>::const_iterator it=this->_SymbolNamesToSymbol.begin();
	      it!=this->_SymbolNamesToSymbol.end(); it++ )
	{
	    string ts = it->first;
	    printf("%s\n", ts.c_str());
	}
    }





};
