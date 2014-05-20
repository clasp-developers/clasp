#ifndef	_sockets_symbolTable_H
#define _sockets_symbolTable_H

#include "core/foundation.h"


namespace kw
{
#define KeywordPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,export) extern core::Symbol_sp cname;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef KeywordPkg_SYMBOLS
}; /* kw package */

namespace sockets
{
#define SocketsPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,export) extern core::Symbol_sp cname;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef SocketsPkg_SYMBOLS
}; /* sockets package */



#endif /* _sockets_symbolTable_H */


