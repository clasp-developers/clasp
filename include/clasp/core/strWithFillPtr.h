/*
    File: strWithFillPtr.h
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
#ifndef _core_strWithFillPtr_H
#define _core_strWithFillPtr_H

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/str.h>
#include <clasp/core/strWithFillPtr.fwd.h>

namespace core {

class StrWithFillPtr_O : public Str_O {
  friend Fixnum &StringFillp(StrWithFillPtr_sp);
  friend T_sp str_out_get_position(T_sp strm);
  LISP_BASE1(Str_O);
  LISP_CLASS(core, CorePkg, StrWithFillPtr_O, "base-string-with-fill-ptr");

protected:
  cl_index _FillPointer;
  bool _Adjustable;

public:
  static StrWithFillPtr_sp create(const string &nm) {
    GC_ALLOCATE(StrWithFillPtr_O, v);
    v->set(nm);
    return v;
  };
  static StrWithFillPtr_sp create(const char *nm) {
    GC_ALLOCATE(StrWithFillPtr_O, v);
    v->setFromChars(nm);
    return v;
  };
  static StrWithFillPtr_sp create(char initial_element, int dimension, int fill_ptr, bool adjustable, T_sp initialContents = _Nil<T_O>());
  /*! Create a buffer BUFFER_STRING_LEN size, fill_ptr=0, adjustable=true */
  static StrWithFillPtr_sp createBufferString(size_t bufferSize = BUFFER_STRING_SIZE) {
    return StrWithFillPtr_O::create(' ', bufferSize, 0, true, _Nil<T_O>());
  };

public:
  /*! For write_array. Really? Should I use the FillPointer? */
  std::vector<cl_index> dimensions() const {
    std::vector<cl_index> dims;
    dims.push_back(this->length());
    return dims;
  };

public:
  virtual bool adjustableArrayP() const { return this->_Adjustable; }
  virtual void set(const string &v) {
    this->Str_O::set(v);
    this->_FillPointer = v.size();
  };
  virtual void setFromChars(const char *v) {
    this->Str_O::set(v);
    this->_FillPointer = this->_Contents.size();
  }
  virtual string get() const { return std::string(this->_Contents.data(), this->_FillPointer); };
  virtual gc::Fixnum size() const { return this->_FillPointer; };
  virtual cl_index fillPointer() const { return this->_FillPointer; };

  bool hasFillPointerP() const { return true; };
  void incrementFillPointer(int offset);
  void setFillPointer(size_t fp);
  bool arrayHasFillPointerP() const { return true; };

  void adjustSize(int adjustment);
  void setSize(int sz);

  /*! Make sure there is enough space from the fill-pointer out */
  void ensureSpaceAfterFillPointer(size_t size);
  /*! Return the address of where the fill_ptr points to */
  char *addressOfFillPtr();
  /*! Adjust the fill pointer */
  void incrementFillPtr(size_t size);

  virtual T_sp vectorPush(T_sp newElement);
  virtual Fixnum_sp vectorPushExtend(T_sp newElement, int extension = 0);

  int pushCharExtend(claspChar c, int extension = 0);

  /*! Push the contents of string designator (str) from (start) to (end) */
  void pushSubString(T_sp str, size_t start, size_t end);

  /*! Push the entire contents of the string in (str) */
  void pushString(T_sp str);

  /*! Push the entire contents of the string in (str) */
  void pushString(const char *str);

  string __repr__() const;

public:
  explicit StrWithFillPtr_O() : Base(), _FillPointer(0), _Adjustable(false){};
  virtual ~StrWithFillPtr_O(){};
};

inline void clasp_string_push_extend(StrWithFillPtr_sp str, Fixnum c) {
  str->pushCharExtend(c);
}
};
template <>
struct gctools::GCInfo<core::StrWithFillPtr_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

TRANSLATE(core::StrWithFillPtr_O);

#endif
