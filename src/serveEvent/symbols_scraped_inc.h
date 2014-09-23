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
#include "serveEvent_scrape_flag.h"
// SYMBOL_TABLE_ENTRY ServeEventPkg    0 ll_serveEventNoTimeout         LL-SERVE-EVENT-NO-TIMEOUT      export ; cName=_sym_ll_serveEventNoTimeout lispName=LL-SERVE-EVENT-NO-TIMEOUT
// SYMBOL_TABLE_ENTRY ServeEventPkg    1 ll_fd_set                      LL-FD-SET                      export ; cName=_sym_ll_fd_set lispName=LL-FD-SET
// SYMBOL_TABLE_ENTRY ServeEventPkg    2 ll_fd_isset                    LL-FD-ISSET                    export ; cName=_sym_ll_fd_isset lispName=LL-FD-ISSET
// SYMBOL_TABLE_ENTRY ServeEventPkg    3 ll_fd_zero                     LL-FD-ZERO                     export ; cName=_sym_ll_fd_zero lispName=LL-FD-ZERO
// SYMBOL_TABLE_ENTRY ServeEventPkg    4 ll_fdset_size                  LL-FDSET-SIZE                  export ; cName=_sym_ll_fdset_size lispName=LL-FDSET-SIZE
// SYMBOL_TABLE_ENTRY ServeEventPkg    5 ll_serveEventWithTimeout       LL-SERVE-EVENT-WITH-TIMEOUT    export ; cName=_sym_ll_serveEventWithTimeout lispName=LL-SERVE-EVENT-WITH-TIMEOUT
// SYMBOL_TABLE_ENTRY ServeEventPkg    6 _PLUS_EINTR_PLUS_              +EINTR+                        export ; cName=_sym__PLUS_EINTR_PLUS_ lispName=+EINTR+
#ifdef ServeEventPkg_SYMBOLS
DO_SYMBOL(_sym_ll_serveEventNoTimeout,0,ServeEventPkg,"LL-SERVE-EVENT-NO-TIMEOUT",true);
DO_SYMBOL(_sym_ll_fd_set,1,ServeEventPkg,"LL-FD-SET",true);
DO_SYMBOL(_sym_ll_fd_isset,2,ServeEventPkg,"LL-FD-ISSET",true);
DO_SYMBOL(_sym_ll_fd_zero,3,ServeEventPkg,"LL-FD-ZERO",true);
DO_SYMBOL(_sym_ll_fdset_size,4,ServeEventPkg,"LL-FDSET-SIZE",true);
DO_SYMBOL(_sym_ll_serveEventWithTimeout,5,ServeEventPkg,"LL-SERVE-EVENT-WITH-TIMEOUT",true);
DO_SYMBOL(_sym__PLUS_EINTR_PLUS_,6,ServeEventPkg,"+EINTR+",true);
#endif
