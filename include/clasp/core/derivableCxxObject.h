/*
    File: derivableCxxObject.h
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
#ifndef DerivableCxxObject_H //[
#define DerivableCxxObject_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>
#include <clasp/core/structureClass.fwd.h>
//#include "model.h"
#include <clasp/core/environment.h>

namespace core {
// Set up this class differently
  SMART(DerivableCxxObject);
  class DerivableCxxObject_O : public Instance_O {
    LISP_META_CLASS(::_lisp->_Roots._TheDerivableCxxClass);
    LISP_CLASS(core, CorePkg, DerivableCxxObject_O, "derivable-cxx-object",Instance_O);
  public:
    static DerivableCxxObject_sp create(T_sp type, List_sp slotNames);
  public:
    explicit DerivableCxxObject_O() : Base(){};
    virtual ~DerivableCxxObject_O(){};
    virtual Fixnum get_stamp_() const = 0;
    virtual size_t get_size_() const = 0;
    virtual Instance_O* get_Instance_O_address_() = 0;
  };
};
#endif //]
