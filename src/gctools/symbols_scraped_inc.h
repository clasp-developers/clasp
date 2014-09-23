/*
    File: symbols_scraped_inc.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
// Symbol table
#include "gctools_scrape_flag.h"
// SYMBOL_TABLE_ENTRY   GcToolsPkg    0 STARallocPatternStackSTAR      *ALLOC-PATTERN-STACK*          export ; cName=_sym_STARallocPatternStackSTAR lispName=*ALLOC-PATTERN-STACK*
// SYMBOL_TABLE_ENTRY   GcToolsPkg    1 maxBootstrapKinds              MAX-BOOTSTRAP-KINDS            export ; cName=_sym_maxBootstrapKinds lispName=MAX-BOOTSTRAP-KINDS
// SYMBOL_TABLE_ENTRY   GcToolsPkg    2 ramp                           RAMP                           export ; cName=_sym_ramp lispName=RAMP
// SYMBOL_TABLE_ENTRY   GcToolsPkg    3 bootstrapKindSymbols           BOOTSTRAP-KIND-SYMBOLS         export ; cName=_sym_bootstrapKindSymbols lispName=BOOTSTRAP-KIND-SYMBOLS
// SYMBOL_TABLE_ENTRY   GcToolsPkg    4 rampCollectAll                 RAMP-COLLECT-ALL               export ; cName=_sym_rampCollectAll lispName=RAMP-COLLECT-ALL
// SYMBOL_TABLE_ENTRY   GcToolsPkg    5 bootstrapKindsP                BOOTSTRAP-KINDS-P              export ; cName=_sym_bootstrapKindsP lispName=BOOTSTRAP-KINDS-P
// SYMBOL_TABLE_ENTRY   GcToolsPkg    6 garbageCollect                 GARBAGE-COLLECT                export ; cName=_sym_garbageCollect lispName=GARBAGE-COLLECT
#ifdef GcToolsPkg_SYMBOLS
DO_SYMBOL(_sym_STARallocPatternStackSTAR,0,GcToolsPkg,"*ALLOC-PATTERN-STACK*",true);
DO_SYMBOL(_sym_maxBootstrapKinds,1,GcToolsPkg,"MAX-BOOTSTRAP-KINDS",true);
DO_SYMBOL(_sym_ramp,2,GcToolsPkg,"RAMP",true);
DO_SYMBOL(_sym_bootstrapKindSymbols,3,GcToolsPkg,"BOOTSTRAP-KIND-SYMBOLS",true);
DO_SYMBOL(_sym_rampCollectAll,4,GcToolsPkg,"RAMP-COLLECT-ALL",true);
DO_SYMBOL(_sym_bootstrapKindsP,5,GcToolsPkg,"BOOTSTRAP-KINDS-P",true);
DO_SYMBOL(_sym_garbageCollect,6,GcToolsPkg,"GARBAGE-COLLECT",true);
#endif
