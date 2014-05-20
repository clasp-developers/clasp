#ifndef	_asttooling_symbolTable_H
#define _asttooling_symbolTable_H

#include "core/foundation.h"


namespace kw
{
#define KeywordPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,export) extern core::Symbol_sp cname;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef KeywordPkg_SYMBOLS
}; /* kw package */

namespace asttooling
{
#define AstToolingPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,export) extern core::Symbol_sp cname;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef AstToolingPkg_SYMBOLS
}; /* asttooling package */



#endif /* _asttooling_symbolTable_H */


