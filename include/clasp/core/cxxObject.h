/*
    File: cxxObject.h
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
#ifndef CxxObject_H
#define CxxObject_H

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>

namespace core {

// set this class up by hand
SMART(CxxObject);
class CxxObject_O : public T_O // StandardObject_O
                    {
  LISP_BASE1(T_O); // LISP_BASE1(StandardObject_O);
  LISP_CLASS(core, CorePkg, CxxObject_O, "CxxObject");

public:
public:
  explicit CxxObject_O() : Base(){};
  virtual ~CxxObject_O(){};
};
};
TRANSLATE(core::CxxObject_O);

#endif
