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
#include "clbind_scrape_flag.h"
// SYMBOL_TABLE_ENTRY    ClbindPkg    0 ClassRep_O                     CLASS-REP                      export ; cName=_sym_ClassRep_O lispName=CLASS-REP
// SYMBOL_TABLE_ENTRY    ClbindPkg    1 STARtheClassRegistrySTAR       *THE-CLASS-REGISTRY*           export ; cName=_sym_STARtheClassRegistrySTAR lispName=*THE-CLASS-REGISTRY*
// SYMBOL_TABLE_ENTRY    ClbindPkg    2 ClassRegistry_O                CLASS-REGISTRY                 export ; cName=_sym_ClassRegistry_O lispName=CLASS-REGISTRY
#ifdef ClbindPkg_SYMBOLS
DO_SYMBOL(_sym_ClassRep_O,0,ClbindPkg,"CLASS-REP",true);
DO_SYMBOL(_sym_STARtheClassRegistrySTAR,1,ClbindPkg,"*THE-CLASS-REGISTRY*",true);
DO_SYMBOL(_sym_ClassRegistry_O,2,ClbindPkg,"CLASS-REGISTRY",true);
#endif
