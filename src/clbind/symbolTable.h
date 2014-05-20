#ifndef	_clbind_symbolTable_H
#define _clbind_symbolTable_H

#include "core/foundation.h"


namespace kw
{
#define KeywordPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,export) extern core::Symbol_sp cname;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef KeywordPkg_SYMBOLS
}; /* kw package */

namespace clbind
{
#define ClbindPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,export) extern core::Symbol_sp cname;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef ClbindPkg_SYMBOLS
}; /* clbind package */



#endif /* _clbind_symbolTable_H */


