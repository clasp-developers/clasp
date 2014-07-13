
#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "symbol.h"
#include "commonLispPackage.h"
#include "multipleValues.h"
#include "package.h"

namespace cl
{

    SYMBOL_EXPORT_SC_(ClPkg,length);
    SYMBOL_EXPORT_SC_(ClPkg,condition);
    SYMBOL_EXPORT_SC_(ClPkg,defvar);
    SYMBOL_EXPORT_SC_(ClPkg,defconstant);
    SYMBOL_EXPORT_SC_(ClPkg,defparameter);
    SYMBOL_EXPORT_SC_(ClPkg,intersection);
    SYMBOL_EXPORT_SC_(ClPkg,union);
    SYMBOL_EXPORT_SC_(ClPkg,remove);
    SYMBOL_EXPORT_SC_(ClPkg,pprint_dispatch);
    SYMBOL_EXPORT_SC_(ClPkg,fileStream);

#pragma GCC visibility push(default)

#define ClPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkgName,lispName,export) core::Symbol_sp cname = UNDEFINED_SYMBOL;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef ClPkg_SYMBOLS

#pragma GCC visibility pop

//    SYMBOL_EXPORT_SC_(ClPkg,defaultPathnameDefaults);

    core::Package_sp initialize_commonLispPackage()
    {
	list<string> lnicknames = {"COMMON-LISP"};
	list<string> luse;
	return _lisp->makePackage("CL",lnicknames,luse);
    }


};

