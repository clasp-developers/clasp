/*
    File: specializer.h
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
#ifndef core_Specializer_H //[
#define core_Specializer_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
//#include "model.h"
#include <clasp/core/metaobject.h>
#include <clasp/core/environment.h>

namespace core {

// Set up this class differently

SMART(Specializer);
class Specializer_O : public Metaobject_O {
  LISP_META_CLASS(StandardClass);
  LISP_BASE1(Metaobject_O);
  LISP_CLASS(core, CorePkg, Specializer_O, "specializer");

public:
  explicit Specializer_O() : Base(){};
  virtual ~Specializer_O(){};
};
};
TRANSLATE(core::Specializer_O);
#endif //]
