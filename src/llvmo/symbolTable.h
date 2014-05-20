#ifndef	_llvmo_symbolTable_H
#define _llvmo_symbolTable_H

#include "core/foundation.h"

namespace kw
{

#define	KeywordPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,export) extern core::Symbol_sp cname;
#include "llvmo/symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef KeywordPkg_SYMBOLS

}; /* kw */

namespace llvmo
{

#define	LlvmoPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,export) extern core::Symbol_sp cname;
#include "llvmo/symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef LlvmoPkg_SYMBOLS

}; /* llvmo */

#endif /* _llvmo_symbolTable_H */


