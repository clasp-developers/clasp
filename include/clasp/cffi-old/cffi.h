/*
    File: cffi.h
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
#ifndef _cffi_H_
#define _cffi_H_

#include <clasp/core/object.h>
#include <clasp/cffi/cffiPackage.h>
#include <clasp/cffi/cffi.fwd.h>

namespace cffi {
class Pointer_O : public core::General_O {
  LISP_CLASS(cffi, CffiPkg, Pointer_O, "Pointer",core::General_O);
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  explicit Pointer_O();
  virtual ~Pointer_O();

public:  // ctor/dtor for classes with shared virtual base
private: // instance variables here
  void *_ptr;

public: // static functions here
  static Pointer_sp null_pointer();
  static Pointer_sp make(core::Number_sp address);
  static Pointer_sp create(void *p);

public: // Functions here
  bool pointerP() const { HARD_IMPLEMENT_ME(); };
  bool null_pointerP() const;

  Pointer_sp inc_pointer(core::Integer_sp offset);

  string __repr__() const;

  /*! Free memory */
  void foreign_free();

  core::T_sp PERCENTmem_ref(core::Symbol_sp atype, core::Integer_sp offset);
  core::T_sp PERCENTsetf_mem_ref(core::Symbol_sp atype, core::Cons_sp rest);

}; // Pointer class

}; // cffi namespace
#endif /* _core_ffi_H_ */
