#ifndef	predefined_symbols_H
#define	predefined_symbols_H

#include "foundation.h"
namespace core
{





extern void initializeAllPredefinedSymbols(Lisp_sp lisp);

#define	PredefinedSymbol_externs
#include "predefinedSymbols_inc.h"

//
//
//    lisp->predefinedSymbol(_sym_kw_renderStyle)
//
//

};
#endif

