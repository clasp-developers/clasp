
#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "symbol.h"
#include "closPackage.h"
#include "multipleValues.h"
#include "package.h"

namespace clos
{

#pragma GCC visibility push(default)
#define ClosPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkgName,lispName,export) core::Symbol_sp cname = UNDEFINED_SYMBOL;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef ClosPkg_SYMBOLS
#pragma GCC visibility pop


    SYMBOL_SC_(ClosPkg,aSingleClosSymbol);



    void initialize_closPackage()
    {
	list<string> lnicknames;
	list<string> luse = {"COMMON-LISP"};
	_lisp->makePackage("CLOS",lnicknames,luse);
	// We don't have to create the CLOS symbols here - it's done in bootStrapCoreSymbolMap
    }


};

