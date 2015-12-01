/*
    File: symbolTable.h
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
#ifndef _core_symbolTable_H
#define _core_symbolTable_H

#include <clasp/core/foundation.h>

namespace cl {
#define ClPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkg, lispname, export) extern core::Symbol_sp cname;
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef ClPkg_SYMBOLS
};

namespace kw {
#define KeywordPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkg, lispname, export) extern core::Symbol_sp cname;
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef KeywordPkg_SYMBOLS
}; /* kw package */

namespace core {
#define CorePkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkg, lispname, export) extern core::Symbol_sp cname;
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef CorePkg_SYMBOLS
}; /* core package */

namespace ext {
#define ExtPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkg, lispname, export) extern core::Symbol_sp cname;
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef ExtPkg_SYMBOLS
}; /* ext package */

namespace comp {
#define CompPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkg, lispname, export) extern core::Symbol_sp cname;
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef CompPkg_SYMBOLS
}; /* comp package */

namespace clos {
#define ClosPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkg, lispname, export) extern core::Symbol_sp cname;
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef ClosPkg_SYMBOLS
}; /* clos package */

namespace cleavirPrimops {
#define CleavirPrimopsPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkg, lispname, export) extern core::Symbol_sp cname;
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef CleavirPrimopsPkg_SYMBOLS
}; /* cleavirPrimops package */

namespace cleavirEnv {
#define CleavirEnvPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkg, lispname, export) extern core::Symbol_sp cname;
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef CleavirEnvPkg_SYMBOLS
}; /* cleavirEnv package */

namespace gray {
#define GrayPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkg, lispname, export) extern core::Symbol_sp cname;
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef GrayPkg_SYMBOLS
}; /* gray package */

#endif /* _core_symbolTable_H */
