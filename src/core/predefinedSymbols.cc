#define	DEBUG_LEVEL_FULL


#include "foundation.h"
#include "lisp.h"


namespace core
{



#define	PredefinedSymbol_storage
#include "predefinedSymbols_inc.h"



void initializeAllPredefinedSymbols(Lisp_sp lisp)
{_G();
#define	PredefinedSymbol_code
#include "predefinedSymbols_inc.h"
}


};
