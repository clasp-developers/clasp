/*
    File: globals.h
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

#ifndef gctools_globals_H
#define gctools_globals_H

#include <clasp/core/foundation.h>
namespace gctools {
/*! Tagged pointer to the global nil */
extern core::Symbol_O *global_tagged_Symbol_OP_nil;
/*! Tagged pointer to the global UNBOUND */
extern core::Symbol_O *global_tagged_Symbol_OP_unbound;
/*! Tagged pointer to the global DELETED - used in weak hash tables */
extern core::Symbol_O *global_tagged_Symbol_OP_deleted;
/*! Tagged pointer to the global SAME-AS-KEY - used in weak hash tables */
extern core::Symbol_O *global_tagged_Symbol_OP_sameAsKey;
};

#endif
