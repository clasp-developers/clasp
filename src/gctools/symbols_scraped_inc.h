// Symbol table
#include "gctools_scrape_flag.h"
// SYMBOL_TABLE_ENTRY   GcToolsPkg    0 bootstrapKindSymbols           BOOTSTRAP-KIND-SYMBOLS         export ; cName=_sym_bootstrapKindSymbols lispName=BOOTSTRAP-KIND-SYMBOLS
// SYMBOL_TABLE_ENTRY   GcToolsPkg    1 maxBootstrapKinds              MAX-BOOTSTRAP-KINDS            export ; cName=_sym_maxBootstrapKinds lispName=MAX-BOOTSTRAP-KINDS
// SYMBOL_TABLE_ENTRY   GcToolsPkg    2 bootstrapKindsP                BOOTSTRAP-KINDS-P              export ; cName=_sym_bootstrapKindsP lispName=BOOTSTRAP-KINDS-P
// SYMBOL_TABLE_ENTRY   GcToolsPkg    3 garbageCollect                 GARBAGE-COLLECT                export ; cName=_sym_garbageCollect lispName=GARBAGE-COLLECT
#ifdef GcToolsPkg_SYMBOLS
DO_SYMBOL(_sym_bootstrapKindSymbols,0,GcToolsPkg,"BOOTSTRAP-KIND-SYMBOLS",true);
DO_SYMBOL(_sym_maxBootstrapKinds,1,GcToolsPkg,"MAX-BOOTSTRAP-KINDS",true);
DO_SYMBOL(_sym_bootstrapKindsP,2,GcToolsPkg,"BOOTSTRAP-KINDS-P",true);
DO_SYMBOL(_sym_garbageCollect,3,GcToolsPkg,"GARBAGE-COLLECT",true);
#endif
