/*
    File: str.h
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
#ifndef _core_str_H
#define _core_str_H

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/character.fwd.h>
#include <clasp/core/lispString.h>

namespace cl {
  extern core::Symbol_sp& _sym_simple_vector;
};

template <>
struct gctools::GCInfo<core::Str_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

FORWARD(Str);
class Str_O : public String_O {
  LISP_CLASS(core, ClPkg, Str_O, "base-string",String_O);
public:
  explicit Str_O(cl_index dim, T_sp fillPointer, T_sp displacedTo, cl_index displacedIndexOffset) : Base(), _Dimension(dim), _FillPointer(fillPointer), _String(), _DisplacedIndexOffset(displacedIndexOffset) {
    // Wipe out the contents
    if (Str_sp ds = displacedTo.asOrNull<Str_O>() ) {
      this->_DisplacedTo = ds;
    } else if ( displacedTo.nilp() ) {
      Str_sp dummy;
      this->_DisplacedTo = dummy;
    } else {
      SIMPLE_ERROR(BF("Displaced-to %s is not a valid initializer") % _rep_(displacedTo));
    };
  }
 public:
  typedef gctools::gcstring str_type;
  typedef gctools::gcstring::value_type element_type;
  typedef char* iterator;
  typedef const char* const_iterator;
 public:
  size_t   _Dimension;
  T_sp     _FillPointer;
  str_type _String;
  Str_sp   _DisplacedTo;
  cl_index _DisplacedIndexOffset;
 public:
  static Str_sp create_with_fill_pointer(char initial_element, size_t dimension, cl_index fill_pointer, bool adjustable);
  static Str_sp createBufferString(size_t bufferSize = BUFFER_STRING_SIZE) {
    return Str_O::create_with_fill_pointer(' ', bufferSize, 0, true);
  };
  static Str_sp create(cl_index dim, Str_sp displacedTo, cl_index displacedIndexOffset );
  static Str_sp create(const string &nm);
  static Str_sp create(const boost::format &nm);
  static Str_sp create(const char *nm);
  static Str_sp create(size_t numChars);
  static Str_sp create(const char *nm, cl_index numChars);
  static Str_sp create(claspChar initial_element, cl_index dimension);
  static Str_sp create(Str_sp orig);
public:
  static Bignum stringToBignum(const char *str);
public:
  virtual bool adjustableArrayP() const { return true; }
public:
  //! dimension() ignores the fill pointer
  virtual cl_index dimension() const { return this->_Dimension; };
  virtual T_sp displaced_to() const {
    if (this->_DisplacedTo) return this->_DisplacedTo;
    return _Nil<T_O>();
  }
  virtual cl_index displaced_index_offset() const {
    return this->_DisplacedIndexOffset;
  }
  //! size is subclassed by Str_O and uses the fill-pointer
  CL_LISPIFY_NAME("core:size");
  CL_DEFMETHOD   virtual cl_index size() const { return this->_Dimension; };
  virtual bool arrayHasFillPointerP() const { return this->_FillPointer.fixnump(); };
  virtual void fillPointerSet(T_sp idx);
  T_sp fillPointer() const { return this->_FillPointer; };
  void unsafe_setf_fill_pointer(T_sp fp) { this->_FillPointer = fp; };
  
  void setSize(cl_index size);
  void ensureSpaceAfterFillPointer(cl_index size);
  char *addressOfFillPointer();
  void incrementFillPointer(cl_index size);

  char &operator[](cl_index i);
  const char &operator[](cl_index i) const;
 
  virtual void swapElements(cl_index i1, cl_index i2) {
    element_type t = (*this)[i2];
    (*this)[i2] = (*this)[i1];
    (*this)[i1] = t;
  }
  virtual T_sp aset_unsafe(cl_index j, T_sp val) {(*this)[j] = val.unsafe_character(); return val; };
  virtual T_sp aref_unsafe(cl_index index) const { return clasp_make_character((*this)[index]); };

  virtual T_sp elementType() const { return cl::_sym_base_char; };
  /*! Return the value at the indices */
  virtual T_sp aref(VaList_sp indices) const;
  /*! Return the value at the indices */
  virtual T_sp setf_aref(List_sp indices_val);

  virtual void __write__(T_sp strm) const;

  virtual T_sp elt(cl_index index) const { return clasp_make_character((*this)[index]);};
  virtual T_sp setf_elt(cl_index index, T_sp value);

  virtual T_sp svref(cl_index index) const { TYPE_ERROR(this->asSmartPtr(), cl::_sym_simple_vector); };
  virtual T_sp setf_svref(cl_index index, T_sp value) { TYPE_ERROR(this->asSmartPtr(), cl::_sym_simple_vector); };

  virtual T_sp subseq(cl_index start, T_sp end) const;
  virtual T_sp setf_subseq(cl_index start, T_sp end, T_sp new_subseq);

  virtual void fillArrayWithElt(T_sp element, Fixnum_sp start, T_sp end);

  virtual void *addressOfBuffer() const;
  virtual size_t elementSizeInBytes() const { return sizeof(claspChar); };

  virtual void fillInitialContents(T_sp initialContents);

  virtual void sxhash_(HashGenerator &hg) const;




//  string __str__() { return this->_Contents.asStdString(); };
  virtual string get() const { string s(this->begin(),this->end()); return s; };
  const char *c_str() const { return this->get().c_str(); };
//  gctools::gcstring &contents() { return this->_Contents; };
  string __repr__() const;
  uint countOccurances(const string &chars);
  List_sp splitAtWhiteSpace();
  List_sp split(const string &splitChars);
//  Fixnum_sp asInt() const;
  Rational_sp parseInteger();
  DoubleFloat_sp asReal() const;
  Str_sp left(cl_index num) const;
  Str_sp right(cl_index num) const;

#if 0
  Str_O &operator+=(const string &s) {
    this->_Contents += s;
    return *this;
  }
  Str_sp operator+(const string &s) {
    string result(this->_Contents.data(), this->_Contents.size());
    result += s;
    return Str_O::create(result);
  }
#endif

  iterator begin() { return &(*this)[0]; };
  iterator end() { return &(*this)[this->length()];};
  const_iterator begin() const { return &(*this)[0]; };
  const_iterator end() const { return &(*this)[this->length()];};
  
  claspChar schar(cl_index index) const;
  claspChar scharSet(cl_index index, claspChar c);

  /*! Return the index of where substring is found 
	  or nil
	*/
  T_sp find(const string &substring, cl_index start);

  virtual T_sp vectorPush(T_sp newElement);
  virtual Fixnum_sp vectorPushExtend(T_sp newElement, cl_index extension = 0);

  /*! Push the contents of string designator (str) from (start) to (end) */
  void pushSubString(T_sp str, cl_index start, cl_index end);

  /*! Push the entire contents of the string in (str) */
  void pushString(T_sp str);

  /*! Push the entire contents of the string in (str) */
  void pushStringCharStar(const char *str);


public:
  cl_index length() const {
    if (this->_FillPointer.fixnump())
      return this->_FillPointer.unsafe_fixnum();
    else return this->_Dimension; };
  
#ifndef USE_TEMPLATE_STRING_MATCHER
  virtual T_sp string_EQ_(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const;
  virtual T_sp string_NE_(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const;
  virtual T_sp string_LT_(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const;
  virtual T_sp string_GT_(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const;
  virtual T_sp string_LE_(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const;
  virtual T_sp string_GE_(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const;

  virtual T_sp string_equal(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const;
  virtual T_sp string_not_equal(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const;
  virtual T_sp string_lessp(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const;
  virtual T_sp string_greaterp(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const;
  virtual T_sp string_not_greaterp(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const;
  virtual T_sp string_not_lessp(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const;
#endif
  
};
};


namespace core {
T_sp core__base_string_concatenate(T_sp vargs);

 #if 0
 // What the hell am I doing here - comment out and see what breaks
inline T_sp base_string_concatenate(LCC_ARGS_ELLIPSIS) {
  VaList_S lcc_arglist_s;
  va_start(lcc_arglist_s._Args, LCC_VA_START_ARG);
  LCC_SPILL_REGISTER_ARGUMENTS_TO_VA_LIST(lcc_arglist_s);
  VaList_sp valist_sp(&lcc_arglist_s);
  return core__base_string_concatenate(valist_sp);
  T_sp replace_array(T_sp other) {
    *this = *gc::As<Str_sp>(other);
    return this->asSmartPtr();
  }
};
#endif


 
inline claspChar clasp_char(Str_sp s, Fixnum pos) { return s->schar(pos); };
};

#endif
