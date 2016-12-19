#define USE_TEMPLATE_STRING_MATCHER
/*
    File: lispString.h
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
#ifndef _core_String_H
#define _core_String_H

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lispVector.h>

namespace core {

FORWARD(String);
class String_O : public Vector_O {
  LISP_CLASS(core, ClPkg, String_O, "String",Vector_O);
public:
  explicit String_O() : Base(){};
 public:
  virtual bool eql_(T_sp obj) const { return this == &*obj;};
  virtual bool equal(T_sp obj) const;
  virtual bool equalp(T_sp obj) const;
  
#ifndef USE_TEMPLATE_STRING_MATCHER
#endif
public:  // Functions here
  virtual cl_index pushCharExtend(claspChar c, cl_index extension = 0) { SUBIMP(); };
  virtual T_sp fillPointer() const { SUBIMP(); };
};
}; /* core */



#endif /* _core_String_H */
