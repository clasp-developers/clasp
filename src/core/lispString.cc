
/*
    File: lispString.cc
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
#define DEBUG_LEVEL_FULL

#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/designators.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispString.h>
#include <clasp/core/character.h>
#include <clasp/core/str.h>
#include <clasp/core/instance.h>
#include <clasp/core/wrappers.h>
namespace core {

static bool member_charbag(claspChar c, SEQUENCE_sp char_bag) {
  if (char_bag.nilp())
    return false;
  if (Cons_sp clcur = char_bag.asOrNull<Cons_O>()) {
    List_sp lcur = clcur;
    for (; lcur.notnilp(); lcur = oCdr(lcur)) {
      if (cl_eql(oCar(lcur), clasp_make_character(c)))
        return true;
    }
  } else if (Vector_sp vcur = char_bag.asOrNull<Vector_O>()) {
    for (size_t i = 0, iEnd(vcur->length()); i < iEnd; ++i) {
      if (cl_eql(vcur->elt(i),
                 clasp_make_character(c)))
        return true;
    }
  }
  return false;
}

static Str_sp string_trim0(bool left_trim, bool right_trim, T_sp char_bag, T_sp tstrng) {
  int i, j;
  Str_sp strng = coerce::stringDesignator(tstrng);
  i = 0;
  j = cl_length(strng);
  if (left_trim) {
    for (; i < j; i++) {
      int c = strng->schar(i);
      if (!member_charbag(c, char_bag))
        break;
    }
  }
  if (right_trim) {
    for (; j > i; j--) {
      int c = strng->schar(j - 1);
      if (!member_charbag(c, char_bag)) {
        break;
      }
    }
  }
  return Str_O::create(strng->substr(i, (j - i)));
}

#define ARGS_cl_stringTrim "(charbag str)"
#define DECL_cl_stringTrim ""
#define DOCS_cl_stringTrim "string_trim"
Str_sp cl_stringTrim(T_sp charbag, T_sp str) {
  _G();
  return string_trim0(true, true, charbag, str);
};

#define ARGS_cl_stringLeftTrim "(charbag str)"
#define DECL_cl_stringLeftTrim ""
#define DOCS_cl_stringLeftTrim "string_left_trim"
Str_sp cl_stringLeftTrim(T_sp charbag, T_sp str) {
  _G();
  return string_trim0(true, false, charbag, str);
};

#define ARGS_cl_stringRightTrim "(charbag str)"
#define DECL_cl_stringRightTrim ""
#define DOCS_cl_stringRightTrim "string_right_trim"
Str_sp cl_stringRightTrim(T_sp charbag, T_sp str) {
  _G();
  return string_trim0(false, true, charbag, str);
};

#define ARGS_cl_string_upcase "(arg)"
#define DECL_cl_string_upcase ""
#define DOCS_cl_string_upcase "string_upcase"
Str_sp cl_string_upcase(T_sp arg) {
  _G();
  Str_sp str = coerce::stringDesignator(arg);
  Str_sp result = Str_O::create(str->get());
  for (Str_O::iterator it = result->begin(); it != result->end(); ++it) {
    *it = toupper(*it);
  }
  return (result);
};

#define ARGS_cl_char "(str idx)"
#define DECL_cl_char ""
#define DOCS_cl_char "char"
claspChar cl_char(T_sp ostr, int idx) {
  _G();
/* Return the character at idx - ignore fill pointers */
#ifdef UNICODE
  IMPLEMENT_MEF(BF("Handle UNICODE"));
#endif
  if (Str_sp str = ostr.asOrNull<Str_O>()) {
    if (idx >= str->size()) {
      TYPE_ERROR_INDEX(str, idx);
    }
    return (*str)[idx];
  }
  QERROR_WRONG_TYPE_NTH_ARG(1, ostr, cl::_sym_string);
  THROW_HARD_ERROR(BF("Should never reach here"));
};

#define ARGS_cl_string_downcase "(arg)"
#define DECL_cl_string_downcase ""
#define DOCS_cl_string_downcase "string_downcase"
Str_sp cl_string_downcase(T_sp arg) {
  _G();
  Str_sp str = coerce::stringDesignator(arg);
  Str_sp result = Str_O::create(str->get());
  for (Str_O::iterator it = result->begin(); it != result->end(); ++it) {
    *it = tolower(*it);
  }
  return (result);
};

#define ARGS_cl_nstring_upcase "(arg)"
#define DECL_cl_nstring_upcase ""
#define DOCS_cl_nstring_upcase "string_upcase"
Str_sp cl_nstring_upcase(T_sp arg) {
  _G();
  Str_sp result = coerce::stringDesignator(arg);
  for (Str_O::iterator it = result->begin(); it != result->end(); ++it) {
    *it = toupper(*it);
  }
  return (result);
};

#define ARGS_cl_nstring_downcase "(arg)"
#define DECL_cl_nstring_downcase ""
#define DOCS_cl_nstring_downcase "string_downcase"
Str_sp cl_nstring_downcase(T_sp arg) {
  _G();
  Str_sp result = coerce::stringDesignator(arg);
  for (Str_O::iterator it = result->begin(); it != result->end(); ++it) {
    *it = tolower(*it);
  }
  return (result);
};

#define ARGS_cl_string "(arg)"
#define DECL_cl_string ""
#define DOCS_cl_string "string"
Str_sp cl_string(T_sp arg) {
  _G();
  Str_sp result = coerce::stringDesignator(arg);
  return (result);
};

bool clasp_memberChar(claspChar c, T_sp charBag) {
  _G();
  if (Str_sp scharBag = charBag.asOrNull<Str_O>()) {
    for (size_t i = 0, iEnd(scharBag->size()); i < iEnd; ++i) {
      if ((*scharBag)[i] == c)
        return true;
    }
    return false;
  }
  SIMPLE_ERROR(BF("Handle clasp_memberChar for %s") % _rep_(charBag));
}

// ----------------------------------------------------------------------
//

EXPOSE_CLASS(core, String_O);

void String_O::exposeCando(::core::Lisp_sp lisp) {
  _G();
  ::core::class_<String_O>()
      //	.initArgs("(self)")
      ;
  SYMBOL_EXPORT_SC_(ClPkg, string);
  ClDefun(string);
  SYMBOL_EXPORT_SC_(ClPkg, string_upcase);
  ClDefun(string_upcase);
  SYMBOL_EXPORT_SC_(ClPkg, string_downcase);
  ClDefun(string_downcase);
  SYMBOL_EXPORT_SC_(ClPkg, nstring_upcase);
  ClDefun(nstring_upcase);
  SYMBOL_EXPORT_SC_(ClPkg, nstring_downcase);
  ClDefun(nstring_downcase);
  SYMBOL_EXPORT_SC_(ClPkg, stringTrim);
  ClDefun(stringTrim);
  SYMBOL_EXPORT_SC_(ClPkg, stringLeftTrim);
  ClDefun(stringLeftTrim);
  SYMBOL_EXPORT_SC_(ClPkg, stringRightTrim);
  ClDefun(stringRightTrim);
  SYMBOL_EXPORT_SC_(ClPkg, char);
  ClDefun(char);
}

void String_O::exposePython(::core::Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(Pkg(), String, "", "", _LISP)
      //	.initArgs("(self)")
    ;
#endif
}

#if defined(XML_ARCHIVE)
void String_O::archiveBase(::core::ArchiveP node) {
  this->Base::archiveBase(node);
  // Archive other instance variables here
}
#endif // defined(XML_ARCHIVE)

void String_O::initialize() {
  this->Base::initialize();
}

#ifdef USE_TEMPLATE_STRING_MATCHER // FooBar
template <typename T>
struct StringCharPointer {
  T _str;
  int _pos;
  int _start;
  typedef char* CharacterType;
  StringCharPointer(Str_sp str, int start) : _str(str), _start(start), _pos(start) {};
  inline int offset() { return this->_pos - this->_start;};
  claspCharacter operator*() { return (claspCharacter)((*this->_str)[this->_pos]);};
  StringCharPointer& operator++() {
    ++this->_pos;
    return *this;
  }
};

/*! bounding index designator range from 0 to the end of each string */
template <typename T1,typename T2>
T_sp template_string_EQ_(T1 string1, T2 string2, int start1, int end1, int start2, int end2)
{
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  int num1 = end1 - start1;
  int num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((*cp1 != *cp2))
      goto RETURN_FALSE;
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
END_STRING1:
  if (num2 == 0)
    goto RETURN_TRUE;
  goto RETURN_FALSE;
END_STRING2:
RETURN_FALSE:
  return _Nil<T_O>();
RETURN_TRUE:
  return _lisp->_true();
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_NE_(T1 string1, T2 string2, int start1, int end1, int start2, int end2)
{
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  int num1 = end1 - start1;
  int num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((*cp1 != *cp2))
      goto RETURN_TRUE;
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
END_STRING1:
  if (num2 != 0)
    goto RETURN_TRUE;
  goto RETURN_FALSE;
END_STRING2: // Did not hit end of string 1 at this point
RETURN_TRUE: // strings are not equal
  return _lisp->_true();
RETURN_FALSE:
  return _Nil<T_O>();
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_LT_(T1 string1, T2 string2, int start1, int end1, int start2, int end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  int num1 = end1 - start1;
  int num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((*cp1 != *cp2)) {
      if (*cp1 < *cp2)
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
END_STRING1:
  if (num2 == 0)
    goto RETURN_FALSE;
RETURN_TRUE:
  return make_fixnum((int)(cp1.offset()));
END_STRING2:
RETURN_FALSE:
  return _Nil<T_O>();
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_GT_(T1 string1, T2 string2, int start1, int end1, int start2, int end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  int num1 = end1 - start1;
  int num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((*cp1 != *cp2)) {
      if (*cp1 > *cp2)
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
END_STRING1:
RETURN_FALSE:
  return _Nil<T_O>();
END_STRING2:
RETURN_TRUE:
  return make_fixnum((int)(cp1.offset()));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_LE_(T1 string1, T2 string2, int start1, int end1, int start2, int end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  int num1 = end1 - start1;
  int num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((*cp1 != *cp2)) {
      if (*cp1 < *cp2)
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
END_STRING1:
  if (num2 == 0)
    goto RETURN_TRUE;
  goto RETURN_FALSE;
END_STRING2:
RETURN_FALSE:
  return _Nil<T_O>();
RETURN_TRUE:
  return make_fixnum((int)(cp1.offset()));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_GE_(T1 string1, T2 string2, int start1, int end1, int start2, int end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  int num1 = end1 - start1;
  int num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((*cp1 != *cp2)) {
      if (*cp1 > *cp2)
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
END_STRING1:
  if (num2 == 0)
    goto RETURN_TRUE;
  goto RETURN_FALSE;
END_STRING2:
RETURN_FALSE:
  return _Nil<T_O>();
RETURN_TRUE:
  return make_fixnum((int)(cp1.offset()));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_equal(T1 string1, T2 string2, int start1, int end1, int start2, int end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  int num1 = end1 - start1;
  int num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((toupper(*cp1) != toupper(*cp2)))
      goto RETURN_FALSE;
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
END_STRING1:
  if (num2 == 0)
    goto RETURN_TRUE;
  goto RETURN_FALSE;
END_STRING2:
RETURN_FALSE:
  return _Nil<T_O>();
RETURN_TRUE:
  return _lisp->_true();
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_not_equal(T1 string1, T2 string2, int start1, int end1, int start2, int end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  int num1 = end1 - start1;
  int num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((toupper(*cp1) != toupper(*cp2)))
      goto RETURN_TRUE;
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
END_STRING1:
  if (num2 != 0)
    goto RETURN_TRUE;
  goto RETURN_FALSE;
END_STRING2: // Did not hit end of string 1 at this point
RETURN_TRUE: // strings are not equal
  return _lisp->_true();
RETURN_FALSE:
  return _Nil<T_O>();
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_lessp(T1 string1, T2 string2, int start1, int end1, int start2, int end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  int num1 = end1 - start1;
  int num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    char ucp1 = toupper(*cp1);
    char ucp2 = toupper(*cp2);
    if (ucp1 != ucp2) {
      if (ucp1 < ucp2)
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
END_STRING1:
  if (num2 == 0)
    goto RETURN_FALSE;
RETURN_TRUE:
  return make_fixnum((int)(cp1.offset()));
END_STRING2:
RETURN_FALSE:
  return _Nil<T_O>();
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_greaterp(T1 string1, T2 string2, int start1, int end1, int start2, int end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  int num1 = end1 - start1;
  int num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    char ucp1 = toupper(*cp1);
    char ucp2 = toupper(*cp2);
    if ((ucp1 != ucp2)) {
      if (ucp1 > ucp2)
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
END_STRING1:
RETURN_FALSE:
  return _Nil<T_O>();
END_STRING2:
RETURN_TRUE:
  return make_fixnum((int)(cp1.offset()));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_not_greaterp(T1 string1, T2 string2, int start1, int end1, int start2, int end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  int num1 = end1 - start1;
  int num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    char ucp1 = toupper(*cp1);
    char ucp2 = toupper(*cp2);
    if ((ucp1 != ucp2)) {
      if (ucp1 < ucp2)
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
END_STRING1:
  if (num2 == 0)
    goto RETURN_TRUE;
  goto RETURN_FALSE;
END_STRING2:
RETURN_FALSE:
  return _Nil<T_O>();
RETURN_TRUE:
  return make_fixnum((int)(cp1.offset()));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_not_lessp(T1 string1, T2 string2, int start1, int end1, int start2, int end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  int num1 = end1 - start1;
  int num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    char ucp1 = toupper(*cp1);
    char ucp2 = toupper(*cp2);
    if ((ucp1 != ucp2)) {
      if (ucp1 > ucp2)
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
END_STRING1:
  if (num2 == 0)
    goto RETURN_TRUE;
  goto RETURN_FALSE;
END_STRING2:
RETURN_FALSE:
  return _Nil<T_O>();
RETURN_TRUE:
  return make_fixnum((int)(cp1.offset()));
}
#endif // FooBar  Way up there USE_TEMPLATE_STRING_MATCHER



inline void setup_string_op_arguments(T_sp string1_desig, T_sp string2_desig,
                                      String_sp &string1, String_sp &string2,
                                      Fixnum_sp start1, T_sp end1,
                                      Fixnum_sp start2, T_sp end2,
                                      int &istart1, int &iend1, int &istart2, int &iend2) {
  _G();
  string1 = coerce::stringDesignator(string1_desig);
  string2 = coerce::stringDesignator(string2_desig);
  istart1 = MAX(unbox_fixnum(start1), 0);
  iend1 = MIN(end1.nilp() ? cl_length(string1) : unbox_fixnum(gc::As<Fixnum_sp>(end1)), cl_length(string1));
  istart2 = MAX(unbox_fixnum(start2), 0);
  iend2 = MIN(end2.nilp() ? cl_length(string2) : unbox_fixnum(gc::As<Fixnum_sp>(end2)), cl_length(string2));
}

#ifndef USE_TEMPLATE_STRING_MATCHER
 #define TEMPLATE_STRING_MATCHER(_string1_,_string2_,_function_,istart1,iend1,istart2,iend2) _string1_->_function_(_string2_,istart1,iend1,istart2,iend2)
#else
#define TEMPLATE_STRING_MATCHER(_string1_,_string2_,_function_,istart1,iend1,istart2,iend2) \
  if ( Str_sp str1 = _string1_.template asOrNull<Str_O>() ) { \
    if ( Str_sp str2 = _string2_.template asOrNull<Str_O>() ) { \
      return _function_<Str_sp,Str_sp>(str1,str2,istart1,iend1,istart2,iend2); \
    } \
  } \
  SIMPLE_ERROR(BF("Illegal combination of string arguments: %s, %s\n") % _rep_(string1) % _rep_(string2) );
#endif





#define ARGS_cl_string_EQ_ "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_cl_string_EQ_ ""
#define DOCS_cl_string_EQ_ "string_EQ_"
T_sp cl_string_EQ_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  _G();
  int istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_EQ_,istart1,iend1,istart2,iend2);
}

#define ARGS_cl_string_NE_ "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_cl_string_NE_ ""
#define DOCS_cl_string_NE_ "string_NE_"
T_mv cl_string_NE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  _G();
  int istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_NE_, istart1, iend1, istart2, iend2);
}

#define ARGS_cl_string_LT_ "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_cl_string_LT_ ""
#define DOCS_cl_string_LT_ "string_LT_"
T_mv cl_string_LT_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  _G();
  int istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_LT_, istart1, iend1, istart2, iend2);
}

#define ARGS_cl_string_GT_ "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_cl_string_GT_ ""
#define DOCS_cl_string_GT_ "string_GT_"
T_mv cl_string_GT_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  _G();
  int istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_GT_, istart1, iend1, istart2, iend2);
}

#define ARGS_cl_string_LE_ "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_cl_string_LE_ ""
#define DOCS_cl_string_LE_ "string_LE_"
T_mv cl_string_LE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  _G();
  int istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_LE_, istart1, iend1, istart2, iend2);
}

#define ARGS_cl_string_GE_ "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_cl_string_GE_ ""
#define DOCS_cl_string_GE_ "string_GE_"
T_mv cl_string_GE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  _G();
  int istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_GE_, istart1, iend1, istart2, iend2);
}

#define ARGS_cl_string_equal "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_cl_string_equal ""
#define DOCS_cl_string_equal "string_equal"
T_sp cl_string_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  _G();
  int istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_equal, istart1, iend1, istart2, iend2);
}

#define ARGS_cl_string_not_equal "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_cl_string_not_equal ""
#define DOCS_cl_string_not_equal "string_not_equal"
T_mv cl_string_not_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  _G();
  int istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_not_equal, istart1, iend1, istart2, iend2);
}
#define ARGS_cl_string_lessp "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_cl_string_lessp ""
#define DOCS_cl_string_lessp "string_lessp"
T_mv cl_string_lessp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  _G();
  int istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_lessp, istart1, iend1, istart2, iend2);
}
#define ARGS_cl_string_greaterp "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_cl_string_greaterp ""
#define DOCS_cl_string_greaterp "string_greaterp"
T_mv cl_string_greaterp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  _G();
  int istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_greaterp, istart1, iend1, istart2, iend2);
}
#define ARGS_cl_string_not_greaterp "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_cl_string_not_greaterp ""
#define DOCS_cl_string_not_greaterp "string_not_greaterp"
T_mv cl_string_not_greaterp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  _G();
  int istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_not_greaterp, istart1, iend1, istart2, iend2);
}
#define ARGS_cl_string_not_lessp "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_cl_string_not_lessp ""
#define DOCS_cl_string_not_lessp "string_not_lessp"
T_mv cl_string_not_lessp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  _G();
  int istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_not_lessp, istart1, iend1, istart2, iend2);
}


bool String_O::equal(T_sp obj) const {
  if ( this == &*obj ) return true;
  if (String_sp s2 = obj.asOrNull<String_O>()) {
    if (this->length() != s2->length()) return false;
    return cl_string_EQ_(this->asSmartPtr(), s2, make_fixnum(0), clasp_make_fixnum(this->length()), make_fixnum(0), clasp_make_fixnum(this->length())).isTrue();
  }
  return false;
}

bool String_O::equalp(T_sp obj) const {
  if (obj.nilp()) return false;
  else if (String_sp s2 = obj.asOrNull<String_O>()) {
    if (this->length() != s2->length())
      return false;
    return cl_string_equal(this->asSmartPtr(), s2, make_fixnum(0), clasp_make_fixnum(this->length()), make_fixnum(0), clasp_make_fixnum(this->length())).isTrue();
  }
  return false;
}


#define ARGS_cl_make_string "(size &key initial-element (element-type 'character))"
#define DECL_cl_make_string ""
#define DOCS_cl_make_string "See CLHS: make_string"
T_mv cl_make_string(Fixnum_sp size, T_sp initial_element, T_sp element_type) {
  _G();
  stringstream ss;
  char ch(' ');
  if (initial_element.notnilp())
    ch = unbox_character(gc::As<Character_sp>(initial_element));
  int isize = unbox_fixnum(size);
  for (int i = 0; i < isize; i++)
    ss << ch;
  Str_sp ns = Str_O::create(ss.str());
  return (Values(ns));
};



#define ARGS_cl_schar "(str index)"
#define DECL_cl_schar ""
#define DOCS_cl_schar "CLHS schar"
claspChar cl_schar(Str_sp str, int idx) {
  if (idx >= 0 && idx < str->length()) {
    return str->schar(idx);
  }
  SIMPLE_ERROR(BF("index %d out of range (0,%d)") % idx % str->length());
};

#define ARGS_core_charSet "(str index c)"
#define DECL_core_charSet ""
#define DOCS_core_charSet "CLHS schar"
claspChar core_charSet(Str_sp str, int idx, claspChar c) {
  if (idx >= 0 && idx < str->length()) {
    str->scharSet(idx, c);
    return c;
  }
  SIMPLE_ERROR(BF("index %d out of range (0,%d)") % idx % str->length());
};

#define ARGS_core_scharSet "(str index c)"
#define DECL_core_scharSet ""
#define DOCS_core_scharSet "CLHS schar"
claspChar core_scharSet(Str_sp str, int idx, claspChar c) {
  if (idx >= 0 && idx < str->length()) {
    str->scharSet(idx, c);
    return c;
  }
  SIMPLE_ERROR(BF("index %d out of range (0,%d)") % idx % str->length());
};

typedef enum { iinit,
               iwhite,
               inum,
               itrailspace,
               ijunk,
               idone } IntegerFSMState;

/*! Digits are 0-9 or a-z/A-Z.
      If digit >= radix then return -1.
     */
int fsmIntegerDigit(char c, int radix) {
  int idigit = -1;
  if (isdigit(c)) {
    idigit = c - '0';
  } else if (isalpha(c)) {
    idigit = -1;
    if (c >= 'A' && c <= 'Z')
      idigit = c - 'A' + 10;
    else if (c >= 'a' && c <= 'z')
      idigit = c - 'a' + 10;
  }
  if (idigit < 0)
    return idigit;
  if (idigit >= radix)
    return -1;
  return idigit;
}

int fsmInteger(mpz_class &result, int &numDigits, bool &sawJunk, const string &str, int istart, int iend, bool junkAllowed, int radix) {
  IntegerFSMState state = iinit;
  int sign = 1;
  result = 0;
  numDigits = 0;
  int cur = istart;
  while (1) {
    char c = str[cur];
    switch (state) {
    case iinit:
    case iwhite: {
      if (isspace(c)) {
        state = iwhite;
        break;
      } else if (c == '-') {
        state = inum;
        sign = -1;
        break;
      } else if (c == '+') {
        state = inum;
        break;
      } else if (isalnum(c)) {
        int idigit = fsmIntegerDigit(c, radix);
        if (idigit < 0 || idigit >= radix) {
          state = ijunk;
          break;
        }
        result = result * radix + idigit;
        ++numDigits;
        state = inum;
        break;
      }
      state = ijunk;
      break;
    }
    case inum: {
      if (isspace(c)) {
        if (junkAllowed) {
          state = itrailspace;
          break;
        }
        state = idone;
        break;
      } else if (isalnum(c)) {
        int idigit = fsmIntegerDigit(c, radix);
        if (idigit < 0 || idigit >= radix) {
          state = ijunk;
          break;
        }
        result = result * radix + idigit;
        ++numDigits;
        state = inum;
        break;
      }
      state = ijunk;
      break;
    }
    case itrailspace: {
      if (junkAllowed) {
        break;
      }
    }
    case ijunk:
      break;
    case idone:
      break;
    }
    if (state == idone)
      break;
    if (state == ijunk)
      break;
    ++cur;
    if (cur >= iend)
      break;
  }
  sawJunk = (state == ijunk);
  if (sign < 0) {
    mpz_class nresult;
    mpz_neg(nresult.get_mpz_t(), result.get_mpz_t());
    mpz_swap(nresult.get_mpz_t(), result.get_mpz_t());
  }
  return cur;
};


#define ARGS_cl_parseInteger "(string &key (start 0) end (radix 10) junk-allowed)"
#define DECL_cl_parseInteger ""
#define DOCS_cl_parseInteger "parseInteger"
T_mv cl_parseInteger(Str_sp str, Fixnum start, T_sp end, uint radix, T_sp junkAllowed) {
  _G();
  Fixnum istart = std::max((Fixnum)0, start);
  Fixnum iend = cl_length(str);
  if (end.notnilp()) {
    iend = std::min(iend, unbox_fixnum(gc::As<Fixnum_sp>(end)));
  }
  mpz_class result;
  bool sawJunk = false;
  int numDigits = 0;
  int cur = fsmInteger(result, numDigits, sawJunk, str->get(), istart, iend, junkAllowed.isTrue(), radix);
  if (junkAllowed.notnilp() || (cur >= iend) || !sawJunk) {
    // normal exit
    if (numDigits > 0) {
      return (Values(Integer_O::create(result), make_fixnum(cur)));
    } else {
      return (Values(_Nil<T_O>(), make_fixnum(cur)));
    }
  }
  PARSE_ERROR(Str_O::create("Could not parse integer from ~S"), Cons_O::create(str));
  UNREACHABLE();
};




void initialize_string()
{
  SYMBOL_EXPORT_SC_(ClPkg, string_EQ_);
  SYMBOL_EXPORT_SC_(ClPkg, string_NE_);
  SYMBOL_EXPORT_SC_(ClPkg, string_LT_);
  SYMBOL_EXPORT_SC_(ClPkg, string_GT_);
  SYMBOL_EXPORT_SC_(ClPkg, string_LE_);
  SYMBOL_EXPORT_SC_(ClPkg, string_GE_);

  SYMBOL_EXPORT_SC_(ClPkg, string_equal);
  SYMBOL_EXPORT_SC_(ClPkg, string_not_equal);
  SYMBOL_EXPORT_SC_(ClPkg, string_lessp);
  SYMBOL_EXPORT_SC_(ClPkg, string_greaterp);
  SYMBOL_EXPORT_SC_(ClPkg, string_not_greaterp);
  SYMBOL_EXPORT_SC_(ClPkg, string_not_lessp);

  ClDefun(string_EQ_);
  ClDefun(string_NE_);
  ClDefun(string_LT_);
  ClDefun(string_GT_);
  ClDefun(string_LE_);
  ClDefun(string_GE_);

  ClDefun(string_equal);
  ClDefun(string_not_equal);
  ClDefun(string_lessp);
  ClDefun(string_greaterp);
  ClDefun(string_not_greaterp);
  ClDefun(string_not_lessp);

  SYMBOL_EXPORT_SC_(ClPkg, make_string);
  ClDefun(make_string);
  core::af_def(ClPkg,"char",&cl_char,ARGS_cl_char,DECL_cl_char,DOCS_cl_char);
  ClDefun(schar);
  CoreDefun(scharSet);
  CoreDefun(charSet);
  ClDefun(parseInteger);
  SYMBOL_EXPORT_SC_(ClPkg, parseInteger);
  ClDefun(parseInteger);
};

}; /* core */
