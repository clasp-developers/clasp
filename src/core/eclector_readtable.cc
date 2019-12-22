/*
    File: eclector_readtable.cc
*/

/*
Copyright (c) 2019, Christian E. Schafmeister
 
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

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbol.h>
#include <clasp/core/eclector_readtable.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/package.h>

namespace eclector_readtable {

SYMBOL_EXPORT_SC_(EclectorReadtablePkg,syntax_type);
SYMBOL_EXPORT_SC_(EclectorReadtablePkg,setf_syntax_type);
SYMBOL_EXPORT_SC_(EclectorReadtablePkg,get_macro_character);
SYMBOL_EXPORT_SC_(EclectorReadtablePkg,set_macro_character);
SYMBOL_EXPORT_SC_(EclectorReadtablePkg,get_dispatch_macro_character);
SYMBOL_EXPORT_SC_(EclectorReadtablePkg,set_dispatch_macro_character);
SYMBOL_EXPORT_SC_(EclectorReadtablePkg,copy_readtable);
SYMBOL_EXPORT_SC_(EclectorReadtablePkg,copy_readtable_into);
SYMBOL_EXPORT_SC_(EclectorReadtablePkg,make_dispatch_macro_character);
SYMBOL_EXPORT_SC_(EclectorReadtablePkg,readtable_case);
SYMBOL_EXPORT_SC_(EclectorReadtablePkg,setf_readtable_case);
SYMBOL_EXPORT_SC_(EclectorReadtablePkg,set_syntax_from_char);
SYMBOL_EXPORT_SC_(EclectorReadtablePkg,readtablep);
};
