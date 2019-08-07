/*
    File: null.cc
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
//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/environment.h>
#include <clasp/core/null.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//


Null_sp Null_O::create_at_boot(const string &nm) {
  // This is used to allocate roots that are pointed
  // to by global variable _sym_XXX  and will never be collected
  T_sp nn = gctools::GC<Null_O>::root_allocate();
  Symbol_sp n = nn;
  n->_Name = SimpleBaseString_O::make(nm.size(),'\0',true,nm.size(),(const claspChar*)nm.c_str());
  return gc::As_unsafe<Null_sp>(n);
};

string Null_O::__repr__() const {
  _OF();
  return "NIL";
}

}; /* core */
