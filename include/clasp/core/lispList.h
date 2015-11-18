/*
    File: lispList.h
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
#ifndef _core_List_H
#define _core_List_H

#include <clasp/core/foundation.h>
#include <clasp/core/sequence.h>
#include <clasp/core/numbers.fwd.h>

namespace core {
T_sp cl_revappend(List_sp tail);
T_sp cl_nreconc(List_sp list, T_sp tail);
T_sp cl_nth(int idx, T_sp arg);
T_sp cl_nthcdr(int idx, T_sp arg);

T_sp cl_copyList(T_sp arg);

T_sp cl_nconc(List_sp rest);

/*! Replace the last CONS of l with y and return l,  if l was nil return y */
T_sp clasp_nconc(T_sp l, T_sp y);

T_sp cl_last(T_sp list, int n = 1);
List_sp cl_nbutlast(List_sp list, Integer_sp n);

void initialize_list();
};

namespace core {
class VaList_dummy_O : public T_O {
  LISP_BASE1(T_O);
  LISP_VIRTUAL_CLASS(core, CorePkg, VaList_dummy_O, "VA-LIST");
};
};
#endif /* _core_List_H */
