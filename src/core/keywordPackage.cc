
#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "metaClass.h"
#include "symbol.h"
#include "keywordPackage.h"
#include "multipleValues.h"
#include "package.h"

namespace kw
{

    SYMBOL_EXPORT_SC_(KeywordPkg,eof);


#pragma GCC visibility push(default)
#define KeywordPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkgName,lispName,export) core::Symbol_sp cname = UNDEFINED_SYMBOL;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef KeywordPkg_SYMBOLS
#pragma GCC visibility pop


    core::Package_sp initialize_keywordPackage()
    {
	list<string> lnicknames = {"KW"};;
	list<string> luse;
	core::Package_sp keywordPackage = _lisp->makePackage("KEYWORD",lnicknames,luse);
	keywordPackage->setKeywordPackage(true);
	return keywordPackage;
    }


};

