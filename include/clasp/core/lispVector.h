/*
    File: lispVector.h
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
#ifndef _core_Vector_H
#define _core_Vector_H

#include <clasp/core/object.h>
#include <clasp/core/array.h>
#include <clasp/core/sequence.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {

FORWARD(Vector);

/*! A one dimensional vector of objects */
// class Vector_O : public Array_O, public T_O
class Vector_O : public Array_O {
  LISP_BASE1(Array_O);
  LISP_CLASS(core, ClPkg, Vector_O, "vector");

public:
  void archiveBase(core::ArchiveP node);

public:
  explicit Vector_O() : Array_O(){};
  virtual ~Vector_O(){};

public:
  void initialize();

private: // instance variables here
public:  // Functions here
  bool equalp(T_sp o) const;
  bool adjustableArrayP() const { return false; };
  gc::Fixnum vector_length() const { return this->dimension(); };
  virtual gc::Fixnum dimension() const { SUBIMP(); };

  virtual T_sp &operator[](uint index) { SUBIMP(); }

  virtual void swapElements(uint idx1, uint idx2) { SUBIMP(); };

  /*! For write_array */
  virtual std::vector<cl_index> dimensions() const { SUBIMP(); };

  virtual size_t elementSizeInBytes() const { SUBIMP(); }
  virtual T_sp elementType() const { SUBIMP(); }
  virtual gc::Fixnum rank() const { return 1; };
  virtual gc::Fixnum arrayDimension(gc::Fixnum axisNumber) const;
  virtual List_sp arrayDimensions() const;
  virtual gc::Fixnum arrayTotalSize() const { return this->length(); };

  virtual T_sp vectorPush(T_sp newElement) { SUBIMP(); };
  virtual Fixnum_sp vectorPushExtend(T_sp newElement, int extension = 1) { SUBIMP(); };

  virtual T_sp aset_unsafe(int j, T_sp val) { SUBIMP(); };
  virtual T_sp aref_unsafe(cl_index index) const { SUBIMP(); };

  virtual cl_index fillPointer() const { SUBIMP(); };
  virtual void setFillPointer(size_t idx) { SUBIMP(); };

  virtual void *addressOfBuffer() const { SUBIMP(); };

  virtual T_sp aref(List_sp indices) const;
  virtual T_sp setf_aref(List_sp indices_val);

  virtual T_sp rowMajorAref(cl_index idx) const { return this->elt(idx); };
  virtual void rowMajorAset(cl_index idx, T_sp value) { this->setf_elt(idx, value); };

  virtual void __write__(T_sp strm) const;

  INHERIT_SEQUENCE virtual gc::Fixnum length() const { return this->dimension(); };
  INHERIT_SEQUENCE virtual T_sp reverse();
  INHERIT_SEQUENCE virtual T_sp nreverse();
  INHERIT_SEQUENCE virtual T_sp elt(int index) const { SUBIMP(); };
  INHERIT_SEQUENCE virtual T_sp setf_elt(int index, T_sp value) { SUBIMP(); };
  INHERIT_SEQUENCE virtual T_sp subseq(int start, T_sp end) const { SUBIMP(); };
  INHERIT_SEQUENCE virtual T_sp setf_subseq(int start, T_sp end, T_sp newSubseq) { SUBIMP(); };

}; /* core */
};
TRANSLATE(core::Vector_O);

namespace cl {
  extern core::Symbol_sp _sym_T_O;
};

namespace core {
// Like ecl_vector_start_end
T_mv clasp_vectorStartEnd(Symbol_sp fn, T_sp thing, Fixnum_sp start, Fixnum_sp end);

Vector_sp core_make_vector(T_sp element_type,
                           int dimension,
                           bool adjustable = false,
                           T_sp fill_pointer = cl::_sym_T_O,
                           T_sp displaced_to = _Nil<T_O>(),
                           T_sp displaced_index_offset = _Nil<T_O>(),
                           T_sp initial_element = _Nil<T_O>(),
                           T_sp initial_contents = _Nil<T_O>());
};
#endif /* _core_Vector_H */
