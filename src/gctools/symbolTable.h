#ifndef	_gctools_symbolTable_H
#define _gctools_symbolTable_H

#include "core/foundation.h"

namespace gctools
{

#define	GcToolsPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,export) extern core::Symbol_sp cname;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef GcToolsPkg_SYMBOLS

}; /* llbmo */


namespace gctoolsTooling
{

#define	GcToolsToolingPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,export) extern core::Symbol_sp cname;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef GcToolsToolingPkg_SYMBOLS

}; /* gctoolsTooling */

namespace gctoolsMatching
{

#define	GcToolsMatchingPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,export) extern core::Symbol_sp cname;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef GcToolsMatchingPkg_SYMBOLS

}; /* gctoolsMatching */

#endif /* _gctools_symbolTable_H */


