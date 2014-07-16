
#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "symbol.h"
#include "commonLispUserPackage.h"
#include "multipleValues.h"
#include "package.h"

namespace cluser
{



#pragma GCC visibility push(default)
#define CommonLispUserPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkgName,lispName,export) core::Symbol_sp cname = UNDEFINED_SYMBOL;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef CommonLispUserPkg_SYMBOLS
#pragma GCC visibility pop



    void initialize_commonLispUserPackage()
    {
	list<string> lnicknames = { "USER", "CL-USER" };
	list<string> luse = { "COMMON-LISP", "CORE" };
	_lisp->makePackage("COMMON-LISP-USER",lnicknames,luse);
	// We don't have to create the COMMONLISPUSER symbols here - it's done in bootStrapCoreSymbolMap
    }


};

