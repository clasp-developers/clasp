/*
    File: array.h
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
#ifndef _core_Array_H
#define _core_Array_H

#include <clasp/core/clasp_gmpxx.h>
#include <clasp/core/foundation.h>
#include <clasp/core/numbers.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {

FORWARD(Array);
class Array_O : public T_O {
  friend class ArrayObjects_O;
  LISP_BASE1(T_O);
  LISP_CLASS(core, ClPkg, Array_O, "array");
#if defined(XML_ARCHIVE)
  DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
#if defined(OLD_SERIALIZE)
  DECLARE_SERIALIZE();
#endif // defined(OLD_SERIALIZE)
public:
  explicit Array_O(){};
  virtual ~Array_O(){};

public:
  void initialize();

private: // instance variables here
public:
  /*! Check an index of the array to make sure it is in bounds */
  static int checkedIndex(const string &filename, int lineno, const string &function, Array_sp array, int which, T_sp index, int nonincl_index);

public: // Functions here
  virtual bool equalp(T_sp other) const;
  virtual T_sp asetUnsafe(int j, T_sp val) { SUBIMP(); };
  virtual bool arrayHasFillPointerP() const { return false; };
  virtual int arrayTotalSize() const;

  virtual void rowMajorAset(int idx, T_sp value) { SUBIMP(); };
  virtual T_sp rowMajorAref(int idx) const { SUBIMP(); };
  virtual int arrayRowMajorIndex(Cons_sp indices) const;

  //! Don't support adjustable arrays yet
  bool adjustable_array_p() const { return false; };
  //! Don't support displaced arrays yet
  bool _displaced_array_p() const { return false; }
  //! Don't support fill pointers yet
  bool array_has_fill_pointer_p() const { return false; }

  virtual LongLongInt setDimensions(Cons_sp dimensions, T_sp initialElement) { SUBIMP(); };

  /*! Return the rank of the array */
  virtual int rank() const { SUBIMP(); };

  /*! Return the offset into a one-dimensional vector for the multidimensional index
      in the vector<int>s.  This is in rowMajor order.*/
  int index_vector_int(const vector<int> &indices) const;

  /*! Return the offset into a one-dimensional vector for a multidimensional index
	 If last_value_is_val == true then don't use the last value in the indices list
	*/
  int index_val(Cons_sp indices, bool last_value_is_val, Cons_sp &val_cons) const;

  /*! Return the offset into a one-dimensional vector for a multidimensional index
	*/
  int index(Cons_sp indices) const;

  /*! Return the type returned by this array */
  virtual T_sp elementType() const {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  };

  /*! This replicates ECL ecl_elttype_to_symbol in array.d */

  Symbol_sp element_type_as_symbol() const;

  /*! Return the array dimension along the axis-number */
  virtual int arrayDimension(int axisNumber) const { SUBIMP(); };

  /*! Return the array dimensions as a list of integers */
  virtual Cons_sp arrayDimensions() const;

  /*! Multiply my contents by a scalar */
  virtual void multiplyByScalar(double scalar) {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  }

  /*! Return the value at the indices */
  virtual T_sp aref(Cons_sp indices) const;

  /*! Setf the value at the indices - the val is at the end of the list of indices */
  virtual T_sp setf_aref(Cons_sp indices_val);

  virtual T_sp svref(int idx) const { SUBIMP(); };
  virtual T_sp setf_svref(int idx, T_sp val) { SUBIMP(); };

  /*! Return the value at the indices */
  virtual void arrayFill(T_sp val) {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  };

  /*! Fill the range of elements of the array,
     if end is nil then fill to the end of the array*/
  virtual void fillArrayWithElt(T_sp element, Fixnum_sp start, T_sp end) {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  };

  virtual string __repr__() const;
};

}; /* core */

TRANSLATE(core::Array_O);

#endif /* _core_Array_H */
