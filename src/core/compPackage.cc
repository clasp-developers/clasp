
#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "symbol.h"
#include "compPackage.h"
#include "multipleValues.h"
#include "package.h"

namespace comp
{

#pragma GCC visibility push(default)
#define CompPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkgName,lispName,export) core::Symbol_sp cname = UNDEFINED_SYMBOL;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef CompPkg_SYMBOLS
#pragma GCC visibility pop


    SYMBOL_SC_(CompPkg,aSingleCompilerSymbol);
    SYMBOL_EXPORT_SC_(CompPkg,STARlowLevelTraceSTAR);
    SYMBOL_EXPORT_SC_(CompPkg,STARlowLevelTracePrintSTAR);

    void initialize_compPackage()
    {
	list<string> lnicknames = { "CMP" };
	list<string> luse = {"COMMON-LISP", "EXT"};
	_lisp->makePackage("COMPILER",lnicknames,luse);
	// We don't have to create the COMPILER symbols here - it's done in bootStrapCoreSymbolMap
    }


};

