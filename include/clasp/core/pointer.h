/*
    File: pointer.h
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
#ifndef _core__pointer_H_
#define _core__pointer_H_

#include <clasp/core/object.h>
#include <clasp/core/pointer.fwd.h>

namespace core {
class Pointer_O : public General_O {
  LISP_CLASS(core, CorePkg, Pointer_O, "Pointer",General_O);
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(Pointer_O);

public: // ctor/dtor for classes with shared virtual base
        //    explicit Pointer_O(core::Instance_sp const& mc) : T_O(mc), T(mc) {};
        //    virtual ~Pointer_O() {};
public:
  void initialize();

private: // instance variables here
  void *_Pointer;

public:
  static Pointer_sp create(void *p);
  /*! Create a pointer to a T_sp shared-ptr */
  static Pointer_sp createForT_sp(T_sp obj);

public: // Functions here
  void *ptr() const { return this->_Pointer; };
  virtual bool eql_(T_sp obj) const;
  bool in_pointer_range(Pointer_sp other, intptr_t size) { return ((char*)this->_Pointer>=(char*)other->_Pointer) && (char*)this->_Pointer<((char*)other->_Pointer+size); };
  bool in_pointer_range(Pointer_sp low, Pointer_sp high) { return ((char*)this->_Pointer>=(char*)low->_Pointer) && (char*)this->_Pointer<((char*)high->_Pointer);  };
  string __repr__() const;

}; // Pointer class

}; // core namespace

#endif /* _core__pointer_H_ */
