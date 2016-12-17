
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
      if (cl__eql(oCar(lcur), clasp_make_character(c)))
        return true;
    }
  } else if (Vector_sp vcur = char_bag.asOrNull<Vector_O>()) {
    for (size_t i = 0, iEnd(vcur->length()); i < iEnd; ++i) {
      if (cl__eql(vcur->elt(i),
                 clasp_make_character(c)))
        return true;
    }
  }
  return false;
}

static Str_sp string_trim0(bool left_trim, bool right_trim, T_sp char_bag, T_sp tstrng) {
  cl_index i, j;
  Str_sp strng = coerce::stringDesignator(tstrng);
  i = 0;
  j = cl__length(strng);
  if (left_trim) {
    for (; i < j; i++) {
      cl_index c = strng->schar(i);
      if (!member_charbag(c, char_bag))
        break;
    }
  }
  if (right_trim) {
    for (; j > i; j--) {
      cl_index c = strng->schar(j - 1);
      if (!member_charbag(c, char_bag)) {
        break;
      }
    }
  }
  return Str_O::create(strng->substr(i, (j - i)));
}

CL_LAMBDA(charbag str);
CL_DECLARE();
CL_DOCSTRING("string_trim");
CL_DEFUN Str_sp cl__string_trim(T_sp charbag, T_sp str) {
  return string_trim0(true, true, charbag, str);
};

CL_LAMBDA(charbag str);
CL_DECLARE();
CL_DOCSTRING("string_left_trim");
CL_DEFUN Str_sp cl__string_left_trim(T_sp charbag, T_sp str) {
  return string_trim0(true, false, charbag, str);
};

CL_LAMBDA(charbag str);
CL_DECLARE();
CL_DOCSTRING("string_right_trim");
CL_DEFUN Str_sp cl__string_right_trim(T_sp charbag, T_sp str) {
  return string_trim0(false, true, charbag, str);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("string_upcase");
CL_DEFUN Str_sp cl__string_upcase(T_sp arg) {
  Str_sp str = coerce::stringDesignator(arg);
  Str_sp result = Str_O::create(str->get());
  for (Str_O::iterator it = result->begin(); it != result->end(); ++it) {
    *it = toupper(*it);
  }
  return (result);
};

CL_LAMBDA(str idx);
CL_DECLARE();
CL_DOCSTRING("char");
CL_DEFUN claspChar cl__char(T_sp ostr, cl_index idx) {
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

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("string_downcase");
CL_DEFUN Str_sp cl__string_downcase(T_sp arg) {
  Str_sp str = coerce::stringDesignator(arg);
  Str_sp result = Str_O::create(str->get());
  for (Str_O::iterator it = result->begin(); it != result->end(); ++it) {
    *it = tolower(*it);
  }
  return (result);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("string_upcase");
CL_DEFUN Str_sp cl__nstring_upcase(T_sp arg) {
  Str_sp result = coerce::stringDesignator(arg);
  for (Str_O::iterator it = result->begin(); it != result->end(); ++it) {
    *it = toupper(*it);
  }
  return (result);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("string_downcase");
CL_DEFUN Str_sp cl__nstring_downcase(T_sp arg) {
  Str_sp result = coerce::stringDesignator(arg);
  for (Str_O::iterator it = result->begin(); it != result->end(); ++it) {
    *it = tolower(*it);
  }
  return (result);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("string");
CL_DEFUN Str_sp cl__string(T_sp arg) {
  Str_sp result = coerce::stringDesignator(arg);
  return (result);
};

bool clasp_memberChar(claspChar c, T_sp charBag) {
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


SYMBOL_EXPORT_SC_(ClPkg, string);
SYMBOL_EXPORT_SC_(ClPkg, string_upcase);
SYMBOL_EXPORT_SC_(ClPkg, string_downcase);
SYMBOL_EXPORT_SC_(ClPkg, nstring_upcase);
SYMBOL_EXPORT_SC_(ClPkg, nstring_downcase);
SYMBOL_EXPORT_SC_(ClPkg, stringTrim);
SYMBOL_EXPORT_SC_(ClPkg, stringLeftTrim);
SYMBOL_EXPORT_SC_(ClPkg, stringRightTrim);
SYMBOL_EXPORT_SC_(ClPkg, char);




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
  cl_index _pos;
  cl_index _start;
  typedef char* CharacterType;
  StringCharPointer(Str_sp str, cl_index start) : _str(str), _start(start), _pos(start) {};
  inline cl_index offset() { return this->_pos - this->_start;};
  claspCharacter operator*() { return (claspCharacter)((*this->_str)[this->_pos]);};
  StringCharPointer& operator++() {
    ++this->_pos;
    return *this;
  }
};

/*! bounding index designator range from 0 to the end of each string */
template <typename T1,typename T2>
T_sp template_string_EQ_(T1 string1, T2 string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2)
{
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  cl_index num1 = end1 - start1;
  cl_index num2 = end2 - start2;
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
T_sp template_string_NE_(T1 string1, T2 string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2)
{
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  cl_index num1 = end1 - start1;
  cl_index num2 = end2 - start2;
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
T_sp template_string_LT_(T1 string1, T2 string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  cl_index num1 = end1 - start1;
  cl_index num2 = end2 - start2;
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
T_sp template_string_GT_(T1 string1, T2 string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  cl_index num1 = end1 - start1;
  cl_index num2 = end2 - start2;
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
T_sp template_string_LE_(T1 string1, T2 string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  cl_index num1 = end1 - start1;
  cl_index num2 = end2 - start2;
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
T_sp template_string_GE_(T1 string1, T2 string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  cl_index num1 = end1 - start1;
  cl_index num2 = end2 - start2;
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
T_sp template_string_equal(T1 string1, T2 string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  cl_index num1 = end1 - start1;
  cl_index num2 = end2 - start2;
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
T_sp template_string_not_equal(T1 string1, T2 string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  cl_index num1 = end1 - start1;
  cl_index num2 = end2 - start2;
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
T_sp template_string_lessp(T1 string1, T2 string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  cl_index num1 = end1 - start1;
  cl_index num2 = end2 - start2;
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
T_sp template_string_greaterp(T1 string1, T2 string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  cl_index num1 = end1 - start1;
  cl_index num2 = end2 - start2;
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
T_sp template_string_not_greaterp(T1 string1, T2 string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  cl_index num1 = end1 - start1;
  cl_index num2 = end2 - start2;
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
T_sp template_string_not_lessp(T1 string1, T2 string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) {
  StringCharPointer<T1> cp1(string1,start1);
  StringCharPointer<T2> cp2(string2,start2);
  cl_index num1 = end1 - start1;
  cl_index num2 = end2 - start2;
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
                                      cl_index &istart1, cl_index &iend1, cl_index &istart2, cl_index &iend2) {
  string1 = coerce::stringDesignator(string1_desig);
  string2 = coerce::stringDesignator(string2_desig);
  istart1 = MAX(unbox_fixnum(start1), 0);
  iend1 = MIN(end1.nilp() ? cl__length(string1) : unbox_fixnum(gc::As<Fixnum_sp>(end1)), cl__length(string1));
  istart2 = MAX(unbox_fixnum(start2), 0);
  iend2 = MIN(end2.nilp() ? cl__length(string2) : unbox_fixnum(gc::As<Fixnum_sp>(end2)), cl__length(string2));
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





CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_EQ_");
CL_DEFUN T_sp cl__string_EQ_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  cl_index istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_EQ_,istart1,iend1,istart2,iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_NE_");
CL_DEFUN T_mv cl__string_NE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  cl_index istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_NE_, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_LT_");
CL_DEFUN T_mv cl__string_LT_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  cl_index istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_LT_, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_GT_");
CL_DEFUN T_mv cl__string_GT_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  cl_index istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_GT_, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_LE_");
CL_DEFUN T_mv cl__string_LE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  cl_index istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_LE_, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_GE_");
CL_DEFUN T_mv cl__string_GE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  cl_index istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_GE_, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_equal");
CL_DEFUN T_sp cl__string_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  cl_index istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_equal, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_not_equal");
CL_DEFUN T_mv cl__string_not_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  cl_index istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_not_equal, istart1, iend1, istart2, iend2);
}
CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_lessp");
CL_DEFUN T_mv cl__string_lessp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  cl_index istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_lessp, istart1, iend1, istart2, iend2);
}
CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_greaterp");
CL_DEFUN T_mv cl__string_greaterp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  cl_index istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_greaterp, istart1, iend1, istart2, iend2);
}
CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_not_greaterp");
CL_DEFUN T_mv cl__string_not_greaterp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  cl_index istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_not_greaterp, istart1, iend1, istart2, iend2);
}
CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_not_lessp");
CL_DEFUN T_mv cl__string_not_lessp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  cl_index istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_MATCHER(string1,string2,template_string_not_lessp, istart1, iend1, istart2, iend2);
}


bool String_O::equal(T_sp obj) const {
  if ( this == &*obj ) return true;
  if (String_sp s2 = obj.asOrNull<String_O>()) {
    if (this->length() != s2->length()) return false;
    return cl__string_EQ_(this->asSmartPtr(), s2, make_fixnum(0), clasp_make_fixnum(this->length()), make_fixnum(0), clasp_make_fixnum(this->length())).isTrue();
  }
  return false;
}

bool String_O::equalp(T_sp obj) const {
  if (obj.nilp()) return false;
  else if (String_sp s2 = obj.asOrNull<String_O>()) {
    if (this->length() != s2->length())
      return false;
    return cl__string_equal(this->asSmartPtr(), s2, make_fixnum(0), clasp_make_fixnum(this->length()), make_fixnum(0), clasp_make_fixnum(this->length())).isTrue();
  }
  return false;
}


CL_LAMBDA("size &key initial-element (element-type 'character)");
CL_DECLARE();
CL_DOCSTRING("See CLHS: make_string");
CL_DEFUN T_mv cl__make_string(Fixnum_sp size, T_sp initial_element, T_sp element_type) {
  stringstream ss;
  char ch(' ');
  if (initial_element.notnilp())
    ch = unbox_character(gc::As<Character_sp>(initial_element));
  cl_index isize = unbox_fixnum(size);
  for (cl_index i = 0; i < isize; i++)
    ss << ch;
  Str_sp ns = Str_O::create(ss.str());
  return (Values(ns));
};



CL_LAMBDA(str index);
CL_DECLARE();
CL_DOCSTRING("CLHS schar");
CL_DEFUN claspChar cl__schar(Str_sp str, cl_index idx) {
  if (idx >= 0 && idx < str->length()) {
    return str->schar(idx);
  }
  SIMPLE_ERROR(BF("index %d out of range (0,%d)") % idx % str->length());
};

CL_LAMBDA(str index c);
CL_DECLARE();
CL_DOCSTRING("CLHS schar");
CL_DEFUN claspChar core__char_set(Str_sp str, cl_index idx, claspChar c) {
  if (idx >= 0 && idx < str->length()) {
    str->scharSet(idx, c);
    return c;
  }
  SIMPLE_ERROR(BF("index %d out of range (0,%d)") % idx % str->length());
};

CL_LAMBDA(str index c);
CL_DECLARE();
CL_DOCSTRING("CLHS schar");
CL_DEFUN claspChar core__schar_set(Str_sp str, cl_index idx, claspChar c) {
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
cl_index fsmIntegerDigit(char c, cl_index radix) {
  cl_index idigit = -1;
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

cl_index fsmInteger(mpz_class &result, cl_index &numDigits, bool &sawJunk, const string &str, cl_index istart, cl_index iend, bool junkAllowed, cl_index radix) {
  IntegerFSMState state = iinit;
  cl_index sign = 1;
  result = 0;
  numDigits = 0;
  cl_index cur = istart;
  while (1) {
    char c = str[cur];
    LOG(BF("fsmInteger str[%ld] -> c = [%d/%c]") % cur  % c % c );
    switch (state) {
      LOG(BF("  top state = %d") % state);
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
        cl_index idigit = fsmIntegerDigit(c, radix);
        if (idigit < 0 || idigit >= radix) {
          LOG(BF("Hit junk at %ld\n") % cur);
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
          state = ijunk; // itrailspace;
          break;
        }
        state = idone;
        break;
      } else if (isalnum(c)) {
        cl_index idigit = fsmIntegerDigit(c, radix);
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
    LOG(BF("  bottom state = %d") % state);
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
  LOG(BF("Returning with cur=%ld") % cur);
  return cur;
};


CL_LAMBDA(string &key (start 0) end (radix 10) junk-allowed);
CL_DECLARE();
CL_DOCSTRING("parseInteger");
CL_DEFUN T_mv cl__parse_integer(Str_sp str, Fixnum start, T_sp end, uint radix, T_sp junkAllowed) {
  Fixnum istart = std::max((Fixnum)0, start);
  Fixnum iend = cl__length(str);
  if (end.notnilp()) {
    iend = std::min(iend, unbox_fixnum(gc::As<Fixnum_sp>(end)));
  }
  mpz_class result;
  bool sawJunk = false;
  cl_index numDigits = 0;
  cl_index cur = fsmInteger(result, numDigits, sawJunk, str->get(), istart, iend, junkAllowed.isTrue(), radix);
  if (junkAllowed.notnilp() || (cur >= iend) || !sawJunk) {
    // normal exit
    if (numDigits > 0) {
      Integer_sp iresult = Integer_O::create(result);
      LOG(BF("Returning parse-integer with result = %s  cur = %ld") % _rep_(iresult) % cur );
      return (Values(iresult, make_fixnum(cur)));
    } else {
      return (Values(_Nil<T_O>(), make_fixnum(cur)));
    }
  }
  PARSE_ERROR(Str_O::create("Could not parse integer from ~S"), Cons_O::create(str));
  UNREACHABLE();
};


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
SYMBOL_EXPORT_SC_(ClPkg, make_string);
SYMBOL_EXPORT_SC_(ClPkg, parseInteger);

}; /* core */
