#ifndef	_mpip_symbolTable_H
#define _mpip_symbolTable_H

#include "core/foundation.h"



namespace kw
{
#define KeywordPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,export) extern core::Symbol_sp cname;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef KeywordPkg_SYMBOLS
}; /* kw package */

namespace mpip
{
#define MpiPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,export) extern core::Symbol_sp cname;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef MpiPkg_SYMBOLS
}; /* mpip package */


#endif /* _mpip_symbolTable_H */


