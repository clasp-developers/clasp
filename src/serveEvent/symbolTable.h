#ifndef	_serveEvent_symbolTable_H
#define _serveEvent_symbolTable_H

#include "core/foundation.h"


namespace kw
{
#define KeywordPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,export) extern core::Symbol_sp cname;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef KeywordPkg_SYMBOLS
}; /* kw package */

namespace serveEvent
{
#define ServeEventPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,export) extern core::Symbol_sp cname;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef ServeEventPkg_SYMBOLS
}; /* serveEvent package */



#endif /* _serveEvent_symbolTable_H */


