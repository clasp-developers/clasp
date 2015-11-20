/*
    File: globals.cc
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
#include <clasp/core/foundation.h>
#include <clasp/gctools/globals.h>

/*! Move all global/static variable definitions into this file so that we can control them
 */
namespace gctools {
/*! Point to the global nil */
core::Symbol_O *global_tagged_Symbol_OP_nil;
/*! Point to the global UNBOUND */
core::Symbol_O *global_tagged_Symbol_OP_unbound;
/*! Point to the global DELETED - used in weak hash tables */
core::Symbol_O *global_tagged_Symbol_OP_deleted;
/*! Point to the global SAME-AS-KEY - used in weak hash tables */
core::Symbol_O *global_tagged_Symbol_OP_sameAsKey;
};
