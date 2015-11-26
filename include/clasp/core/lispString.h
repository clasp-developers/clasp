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
  LISP_BASE1(Vector_O);
  LISP_CLASS(core, ClPkg, String_O, "String");

  DECLARE_INIT();
#if defined(XML_ARCHIVE)
  DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
public:
  explicit String_O() : Base(){};
  virtual ~String_O(){};

public:
  void initialize();

 private: // instance variables here
 public:
  virtual bool eql_(T_sp obj) const { return this == &*obj;};
  virtual bool equal(T_sp obj) const;
  virtual bool equalp(T_sp obj) const;
  
#ifndef USE_TEMPLATE_STRING_MATCHER
  virtual T_sp string_EQ_(Str_sp string2, int start1, int end1, int start2, int end2) const{SUBIMP();};
  virtual T_sp string_NE_(Str_sp string2, int start1, int end1, int start2, int end2) const{SUBIMP();};
  virtual T_sp string_LT_(Str_sp string2, int start1, int end1, int start2, int end2) const{SUBIMP();};
  virtual T_sp string_GT_(Str_sp string2, int start1, int end1, int start2, int end2) const{SUBIMP();};
  virtual T_sp string_LE_(Str_sp string2, int start1, int end1, int start2, int end2) const{SUBIMP();};
  virtual T_sp string_GE_(Str_sp string2, int start1, int end1, int start2, int end2) const{SUBIMP();};

  virtual T_sp string_equal(Str_sp string2, int start1, int end1, int start2, int end2) const{SUBIMP();};
  virtual T_sp string_not_equal(Str_sp string2, int start1, int end1, int start2, int end2) const{SUBIMP();};
  virtual T_sp string_lessp(Str_sp string2, int start1, int end1, int start2, int end2) const{SUBIMP();};
  virtual T_sp string_greaterp(Str_sp string2, int start1, int end1, int start2, int end2) const{SUBIMP();};
  virtual T_sp string_not_greaterp(Str_sp string2, int start1, int end1, int start2, int end2) const{SUBIMP();};
  virtual T_sp string_not_lessp(Str_sp string2, int start1, int end1, int start2, int end2) const{SUBIMP();};
#endif
public:  // Functions here
  virtual int pushCharExtend(claspChar c, int extension = 0) { SUBIMP(); };
  virtual cl_index fillPointer() const { SUBIMP(); };
};

 
}; /* core */

TRANSLATE(core::String_O);

namespace core {

Str_sp cl_string(T_sp str);

Str_sp cl_string_upcase(T_sp arg);
Str_sp cl_string_downcase(T_sp arg);

claspChar cl_char(T_sp str, int idx);

bool clasp_memberChar(claspChar c, T_sp charBag);

Str_sp cl_stringTrim(T_sp charbag, T_sp str);
Str_sp cl_stringLeftTrim(T_sp charbag, T_sp str);
Str_sp cl_stringRightTrim(T_sp charbag, T_sp str);

 T_mv cl_parseInteger(Str_sp str, Fixnum start = 0, T_sp end = _Nil<T_O>(), uint radix = 10, T_sp junkAllowed = _Nil<T_O>());

 T_sp cl_string_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1 = make_fixnum(0), T_sp end1 = _Nil<T_O>(), Fixnum_sp start2 = make_fixnum(0), T_sp end2 = _Nil<T_O>());


 void initialize_string();

};
#endif /* _core_String_H */
