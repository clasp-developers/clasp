/*
    File: metaobject.h
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
#ifndef core_Metaobject_H //[
#define core_Metaobject_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
//#include "model.h"
#include <clasp/core/executables.fwd.h>
#include <clasp/core/lisp.h>

#include <clasp/core/standardObject.h>
#include <clasp/core/environment.h>

namespace core {

// Set up this class differently

SMART(Metaobject);
class Metaobject_O : public StandardObject_O {
  LISP_META_CLASS(StandardClass);
  LISP_BASE1(StandardObject_O);
  LISP_CLASS(core, CorePkg, Metaobject_O, "metaobject");

public:
  explicit Metaobject_O();
  virtual ~Metaobject_O(){};
};
};
TRANSLATE(core::Metaobject_O);
#endif //]
