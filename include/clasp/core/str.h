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
#include <clasp/core/lispString.h>

namespace core {

FORWARD(Str);
class Str_O : public String_O {
  LISP_BASE1(String_O);
  LISP_CLASS(core, ClPkg, Str_O, "base-string");

public:
#if defined(OLD_SERIALIZE)
  void serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
  void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
GCPROTECTED:
  typedef gctools::gcstring str_type;
  str_type _Contents;

public:
  typedef str_type::iterator iterator;
  typedef str_type::const_iterator const_iterator;

public:
  static Str_sp create(const boost::format &fmt);
  static Str_sp create(const string &nm);
  static Str_sp create(const char *nm);
  static Str_sp create(const char *nm, int numChars);
  static Str_sp create(claspChar initial_element, int dimension, T_sp initialContents);
  static Str_sp create(Str_sp orig);

public:
  static Bignum stringToBignum(const char *str);

public:
  virtual bool adjustableArrayP() const { return false; }

public:
  //! dimension() ignores the fill pointer
  virtual uint dimension() const { return this->_Contents.size(); };
  //! size is subclassed by StrWithFillPtr_O and uses the fill-pointer
  virtual uint size() const { return this->_Contents.size(); };
  const char *c_str() const { return this->_Contents.c_str(); };
  string __str__() { return this->_Contents.asStdString(); };
  virtual string get() const { return this->_Contents.asStdString(); };
  virtual void set(const string &v) {
    str_type temp(v);
    this->_Contents.swap(temp);
  };
  virtual void setFromChars(const char *v) {
    str_type temp(v);
    this->_Contents.swap(temp);
  };
  virtual void setFromChars(const char *v, int num) {
    str_type temp(v, num);
    this->_Contents.swap(temp);
  };
  virtual T_sp asetUnsafe(int j, T_sp val);
  gctools::gcstring &contents() { return this->_Contents; };
  string __repr__() const;
  uint countOccurances(const string &chars);
  Cons_sp splitAtWhiteSpace();
  Cons_sp split(const string &splitChars);
  char &operator[](int i) { return this->_Contents[i]; };
  const char &operator[](int i) const { return this->_Contents[i]; };
  Fixnum_sp asInt() const;
  Rational_sp parseInteger();
  DoubleFloat_sp asReal() const;
  Symbol_sp asSymbol() const;
  Symbol_sp asKeywordSymbol() const;
  string left(int num) const;
  string right(int num) const;
  string substr(int start, int num) const;
  void sxhash(HashGenerator &hg) const;

  Str_O &operator+=(const string &s) {
    this->_Contents += s;
    return *this;
  }

  Str_sp operator+(const string &s) {
    string result(this->_Contents.data(), this->_Contents.size());
    result += s;
    return Str_O::create(result);
  }

  iterator begin() { return this->_Contents.data(); }
  iterator end() { return this->_Contents.data() + this->size(); };
  const_iterator begin() const { return this->_Contents.data(); }
  const_iterator end() const { return this->_Contents.data() + this->size(); };

  claspChar schar(int index) const;
  claspChar scharSet(int index, claspChar c);

  /*! Return the index of where substring is found 
	  or nil
	*/
  Rational_sp find(const string &substring, int start);

public:
  //! dim ignore fill pointers - don't overload
  uint length() const { return this->size(); };
  //	T_sp prim_format(Function_sp e, Cons_sp args, Environment_sp environ, Lisp_sp lisp );
  //	T_sp prim_formatCons(Function_sp e, Cons_sp args, Environment_sp environ, Lisp_sp lisp );
  virtual T_sp elementType() const;
  virtual bool equal(T_sp obj) const;
  virtual bool equalp(T_sp obj) const;
  virtual bool eql(T_sp obj) const;
  virtual bool operator<(T_sp obj) const;
  virtual bool operator<=(T_sp obj) const;
  virtual bool operator>(T_sp obj) const;
  virtual bool operator>=(T_sp obj) const;

  virtual T_sp string_EQ_(Str_sp string2, int start1, int end1, int start2, int end2) const;
  virtual T_sp string_NE_(Str_sp string2, int start1, int end1, int start2, int end2) const;
  virtual T_sp string_LT_(Str_sp string2, int start1, int end1, int start2, int end2) const;
  virtual T_sp string_GT_(Str_sp string2, int start1, int end1, int start2, int end2) const;
  virtual T_sp string_LE_(Str_sp string2, int start1, int end1, int start2, int end2) const;
  virtual T_sp string_GE_(Str_sp string2, int start1, int end1, int start2, int end2) const;

  virtual T_sp string_equal(Str_sp string2, int start1, int end1, int start2, int end2) const;
  virtual T_sp string_not_equal(Str_sp string2, int start1, int end1, int start2, int end2) const;
  virtual T_sp string_lessp(Str_sp string2, int start1, int end1, int start2, int end2) const;
  virtual T_sp string_greaterp(Str_sp string2, int start1, int end1, int start2, int end2) const;
  virtual T_sp string_not_greaterp(Str_sp string2, int start1, int end1, int start2, int end2) const;
  virtual T_sp string_not_lessp(Str_sp string2, int start1, int end1, int start2, int end2) const;

  /*! Return the value at the indices */
  virtual T_sp aref(Cons_sp indices) const;
  /*! Return the value at the indices */
  virtual T_sp setf_aref(Cons_sp indices_val);

  virtual void __write__(T_sp strm) const;

  virtual T_sp elt(int index) const;
  virtual T_sp setf_elt(int index, T_sp value);

  virtual T_sp svref(int index) const { return elt(index); };
  virtual T_sp setf_svref(int index, T_sp value) { return this->setf_elt(index, value); };

  virtual T_sp subseq(int start, T_sp end) const;
  virtual T_sp setf_subseq(int start, T_sp end, T_sp new_subseq);

  virtual void fillArrayWithElt(T_sp element, Fixnum_sp start, T_sp end);

  virtual void *addressOfBuffer() const;
  virtual size_t elementSizeInBytes() const { return sizeof(claspChar); };

  virtual void fillInitialContents(T_sp initialContents);

public:
  explicit Str_O() : Base(){};
  virtual ~Str_O();
};
};
template <>
struct gctools::GCInfo<core::Str_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static bool constexpr Moveable = true;
  static bool constexpr Atomic = false;
};

TRANSLATE(core::Str_O);

namespace core {
T_mv af_parseInteger(Str_sp str, uint start = 0, T_sp end = _Nil<T_O>(), uint radix = 10, T_sp junkAllowed = _Nil<T_O>());
T_sp af_string_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1 = Fixnum_O::create(0), Fixnum_sp end1 = _Nil<Fixnum_O>(), Fixnum_sp start2 = Fixnum_O::create(0), Fixnum_sp end2 = _Nil<Fixnum_O>());

T_sp af_base_string_concatenate(Cons_sp args);

inline claspChar clasp_char(Str_sp s, Fixnum pos) { return s->schar(pos); };
};

#endif
