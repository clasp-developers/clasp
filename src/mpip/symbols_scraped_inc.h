/*
    File: symbols_scraped_inc.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
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
#include "mpip_scrape_flag.h"
// SYMBOL_TABLE_ENTRY       MpiPkg    0 _PLUS_anyTag_PLUS_             +ANY-TAG+                      export ; cName=_sym__PLUS_anyTag_PLUS_ lispName=+ANY-TAG+
// SYMBOL_TABLE_ENTRY       MpiPkg    1 MpiTermConverter               MPI-TERM-CONVERTER             export ; cName=_sym_MpiTermConverter lispName=MPI-TERM-CONVERTER
// SYMBOL_TABLE_ENTRY       MpiPkg    2 Mpi_O                          MPI                            export ; cName=_sym_Mpi_O lispName=MPI
// SYMBOL_TABLE_ENTRY       MpiPkg    3 _PLUS_anySource_PLUS_          +ANY-SOURCE+                   export ; cName=_sym__PLUS_anySource_PLUS_ lispName=+ANY-SOURCE+
// SYMBOL_TABLE_ENTRY       MpiPkg    4 STARworldSTAR                  *WORLD*                        export ; cName=_sym_STARworldSTAR lispName=*WORLD*
#ifdef MpiPkg_SYMBOLS
DO_SYMBOL(_sym__PLUS_anyTag_PLUS_,0,MpiPkg,"+ANY-TAG+",true);
DO_SYMBOL(_sym_MpiTermConverter,1,MpiPkg,"MPI-TERM-CONVERTER",true);
DO_SYMBOL(_sym_Mpi_O,2,MpiPkg,"MPI",true);
DO_SYMBOL(_sym__PLUS_anySource_PLUS_,3,MpiPkg,"+ANY-SOURCE+",true);
DO_SYMBOL(_sym_STARworldSTAR,4,MpiPkg,"*WORLD*",true);
#endif
