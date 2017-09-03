/*
    File: macros.cc
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
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <setfExpander.h>

namespace core {

namespace macros {

T_sp setf(Symbol_sp accessor, T_sp target, T_sp val, Lisp_sp lisp) {
  SetfExpander_sp expander = _lisp->lookupSetfExpander(accessor);
  return expander->invoke(target, val);
}

T_sp push(Symbol_sp accessor, T_sp target, T_sp val, Lisp_sp lisp) {
  SetfExpander_sp expander = _lisp->lookupSetfExpander(accessor);
  List_sp tlist = target->slot_value(accessor);
  tlist = Cons_O::create(val, tlist);
  return expander->invoke(target, tlist);
}

T_sp pushnew(Symbol_sp accessor, T_sp target, T_sp val, T_sp test_funcDesig, T_sp key_funcDesig, Lisp_sp lisp) {
  IMPLEMENT_MEF("implement macros::pushnew");
#if 0
	SetfExpander_sp expander = _lisp->lookupSetfExpander(accessor);
	Cons_sp tlist = target->slot_value(accessor);
	for ( Cons_sp cur =tlist; cur.notnilp(); cur=cCdr(cur) )
	{
	    WORKING WORKING WORKING 
	tlist = Cons_O::create(val,tlist);
	return expander->invoke(target,tlist);
#endif
}

p
}; // namespace macros
}; // namespace core
