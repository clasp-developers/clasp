#include <string.h>
#include <clasp/core/foundation.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/bformat.h>
#include <clasp/core/designators.h>
#include <clasp/core/array.h>
#include <clasp/core/character.h>

// ----------------------------------------------------------------------
//
//  String functions
//

namespace core {

static bool member_charbag(claspCharacter c, SEQUENCE_sp char_bag) {
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
      if (cl__eql(vcur->rowMajorAref(i),clasp_make_character(c)))
        return true;
    }
  }
  return false;
}

static String_sp string_trim0(bool left_trim, bool right_trim, T_sp char_bag, T_sp tstrng) {
  cl_index i, j;
  String_sp strng = coerce::stringDesignator(tstrng);
  i = 0;
  j = cl__length(strng);
  if (left_trim) {
    for (; i < j; i++) {
      claspCharacter c = cl__char(strng,i).unsafe_character();
      if (!member_charbag(c, char_bag))
        break;
    }
  }
  if (right_trim) {
    for (; j > i; j--) {
      claspCharacter c = cl__char(strng,j - 1).unsafe_character();
      if (!member_charbag(c, char_bag)) {
        break;
      }
    }
  }
  return strng->unsafe_subseq(i, j);
}

CL_LAMBDA(charbag str);
CL_DECLARE();
CL_DOCSTRING("string_trim");
CL_DEFUN String_sp cl__string_trim(T_sp charbag, T_sp str) {
  return string_trim0(true, true, charbag, str);
};

CL_LAMBDA(charbag str);
CL_DECLARE();
CL_DOCSTRING("string_left_trim");
CL_DEFUN String_sp cl__string_left_trim(T_sp charbag, T_sp str) {
  return string_trim0(true, false, charbag, str);
};

CL_LAMBDA(charbag str);
CL_DECLARE();
CL_DOCSTRING("string_right_trim");
CL_DEFUN String_sp cl__string_right_trim(T_sp charbag, T_sp str) {
  return string_trim0(false, true, charbag, str);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("string");
CL_DEFUN String_sp cl__string(T_sp arg) {
  String_sp result = coerce::stringDesignator(arg);
  return (result);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("string_upcase");
CL_DEFUN SimpleString_sp cl__string_upcase(T_sp arg) {
  String_sp str = coerce::stringDesignator(arg);
  SimpleString_sp result = gc::As_unsafe<SimpleString_sp>(core__make_vector(str->element_type(),str->length(),false));
  for ( size_t i(0), iEnd(str->length()); i<iEnd; ++i ) {
    T_sp cc = str->rowMajorAref(i);
    claspCharacter c = cc.unsafe_character();
    claspCharacter u = claspCharacter_upcase(c);
    Character_sp cu = clasp_make_character(u);
    result->rowMajorAset(i,clasp_make_character(u));
  }
  return (result);
};


CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("string_downcase");
CL_DEFUN SimpleString_sp cl__string_downcase(T_sp arg) {
  String_sp str = coerce::stringDesignator(arg);
  SimpleString_sp result = gc::As_unsafe<SimpleString_sp>(core__make_vector(str->element_type(),str->length(),false));
  for ( size_t i(0), iEnd(str->length()); i<iEnd; ++i ) {
    claspCharacter c = str->rowMajorAref(i).unsafe_character();
    claspCharacter u = claspCharacter_downcase(c);
    result->rowMajorAset(i,clasp_make_character(u));
  }
  return (result);
};


CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("nstring_upcase");
CL_DEFUN String_sp cl__nstring_upcase(String_sp arg) {
  for ( cl_index i(0), iEnd(arg->length()); i<iEnd; ++i ) {
    arg->rowMajorAset(i,clasp_make_character(claspCharacter_upcase(arg->rowMajorAref(i).unsafe_character())));
  }
  return arg;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("nstring_downcase");
CL_DEFUN String_sp cl__nstring_downcase(String_sp arg) {
  for ( cl_index i(0), iEnd(arg->length()); i<iEnd; ++i ) {
    arg->rowMajorAset(i,clasp_make_character(claspCharacter_downcase(arg->rowMajorAref(i).unsafe_character())));
  }
  return arg;
};



bool clasp_memberChar(claspChar c, String_sp charBag) {
  for ( cl_index i(0), iEnd(charBag->length()); i<iEnd; ++i ) {
    if (charBag->rowMajorAref(i).unsafe_character() == c) return true;
  }
  return false;
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

template <typename T>
struct StringCharPointer {
  const T* _stringPtr;
  size_t _pos;
  size_t _start;
  typedef typename T::simple_element_type CharacterType;
  StringCharPointer(const T* strP, size_t start) : _stringPtr(strP), _start(start), _pos(start) {}
  inline size_t offset() { return this->_pos - this->_start;};
  CharacterType operator*() {
    CharacterType c = (*this->_stringPtr)[this->_pos];
    return c;
  }
void* address() { return (void*)&((*this->_stringPtr)[this->_pos]);}
StringCharPointer& operator++() {
  ++this->_pos;
  return *this;
}
};


template <typename T1, typename T2>
bool template_string_equalp_bool(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((toupper(static_cast<claspCharacter>(*cp1)) != toupper(static_cast<claspCharacter>(*cp2))))
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
  return false;
 RETURN_TRUE:
  return true;
}




/*! bounding index designator range from 0 to the end of each string */
template <typename T1,typename T2>
T_sp template_string_EQ_(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2)
{
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((static_cast<claspCharacter>(*cp1) != static_cast<claspCharacter>(*cp2)))
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
T_sp template_string_NE_(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2)
{
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((static_cast<claspCharacter>(*cp1) != static_cast<claspCharacter>(*cp2)))
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
  return make_fixnum((int)(cp1.offset() + start1));
 RETURN_FALSE:
  return _Nil<T_O>();
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_LT_(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((static_cast<claspCharacter>(*cp1) != static_cast<claspCharacter>(*cp2))) {
      if (static_cast<claspCharacter>(*cp1) < static_cast<claspCharacter>(*cp2))
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
  return make_fixnum((int)(cp1.offset() + start1));
 END_STRING2:
 RETURN_FALSE:
  return _Nil<T_O>();
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_GT_(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((static_cast<claspCharacter>(*cp1) != static_cast<claspCharacter>(*cp2))) {
      if (static_cast<claspCharacter>(*cp1) > static_cast<claspCharacter>(*cp2))
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
  return make_fixnum((int)(cp1.offset() + start1));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_LE_(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  // the empty string is le any other string
  if (num1 == 0) goto RETURN_TRUE;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((static_cast<claspCharacter>(*cp1) != static_cast<claspCharacter>(*cp2))) {
      if (static_cast<claspCharacter>(*cp1) < static_cast<claspCharacter>(*cp2))
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
 END_STRING1:
    goto RETURN_TRUE;
 END_STRING2:
 RETURN_FALSE:
  return _Nil<T_O>();
 RETURN_TRUE:
  return make_fixnum((int)(cp1.offset() + start1));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_GE_(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  // Any String is ge the empty string
  if (num2 == 0) goto RETURN_TRUE;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((static_cast<claspCharacter>(*cp1) != static_cast<claspCharacter>(*cp2))) {
      if (static_cast<claspCharacter>(*cp1) > static_cast<claspCharacter>(*cp2))
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
  // String1 still has chars, string2 not
  goto RETURN_TRUE;
 RETURN_FALSE:
  return _Nil<T_O>();
 RETURN_TRUE:
  return make_fixnum((int)(cp1.offset() + start1));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_equal(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((toupper(static_cast<claspCharacter>(*cp1)) != toupper(static_cast<claspCharacter>(*cp2))))
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
T_sp template_string_not_equal(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((toupper(static_cast<claspCharacter>(*cp1)) != toupper(static_cast<claspCharacter>(*cp2))))
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
  return make_fixnum((int)(cp1.offset() + start1)); //_lisp->_true();
 RETURN_FALSE:
  return _Nil<T_O>();
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_lessp(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    claspCharacter ucp1 = toupper(static_cast<claspCharacter>(*cp1));
    claspCharacter ucp2 = toupper(static_cast<claspCharacter>(*cp2));
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
  return make_fixnum((int)(cp1.offset() + start1));
 END_STRING2:
 RETURN_FALSE:
  return _Nil<T_O>();
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_greaterp(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    claspCharacter ucp1 = toupper(static_cast<claspCharacter>(*cp1));
    claspCharacter ucp2 = toupper(static_cast<claspCharacter>(*cp2));
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
  return make_fixnum((int)(cp1.offset() + start1));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_not_greaterp(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  // The empty String is not greater than any other string, even another empty string
  if (num1 == 0) goto RETURN_TRUE;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    claspCharacter ucp1 = toupper(static_cast<claspCharacter>(*cp1));
    claspCharacter ucp2 = toupper(static_cast<claspCharacter>(*cp2));
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
    goto RETURN_TRUE;
 END_STRING2:
 RETURN_FALSE:
  return _Nil<T_O>();
 RETURN_TRUE:
  return make_fixnum((int)(cp1.offset() + start1));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_not_lessp(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  // No String is lessp the empty string
  // So every String is not-lessp the empty string
  if (num2 == 0) goto RETURN_TRUE;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    claspCharacter ucp1 = toupper(static_cast<claspCharacter>(*cp1));
    claspCharacter ucp2 = toupper(static_cast<claspCharacter>(*cp2));
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
  //String2 is consumed, String1 not yet
  goto RETURN_TRUE;
 RETURN_FALSE:
  return _Nil<T_O>();
 RETURN_TRUE:
  return make_fixnum((int)(cp1.offset() + start1));
}

inline void setup_string_op_arguments(T_sp string1_desig, T_sp string2_desig,
                                      String_sp &string1, String_sp &string2,
                                      Fixnum_sp start1, T_sp end1,
                                      Fixnum_sp start2, T_sp end2,
                                      size_t &istart1, size_t &iend1,
                                      size_t &istart2, size_t &iend2) {
  string1 = coerce::stringDesignator(string1_desig);
  string2 = coerce::stringDesignator(string2_desig);
  Fixnum fstart1 = unbox_fixnum(start1);
  if (fstart1 < 0)
      // a negative start1 should error
    SIMPLE_ERROR(BF("start1 %d out of bounds for string %s") % fstart1 % string1);
  istart1 = MAX(fstart1, 0);
  if (istart1 > cl__length(string1)) {
    SIMPLE_ERROR(BF("start1 %d out of bounds for string %s") % istart1 % string1);
  }
  iend1 = MIN(end1.nilp() ? cl__length(string1) : unbox_fixnum(gc::As<Fixnum_sp>(end1)), cl__length(string1));
  Fixnum fstart2 = unbox_fixnum(start2);
  if (fstart2 <0)
    // a negative start2 should error
    SIMPLE_ERROR(BF("start2 %d out of bounds for string %s") % fstart2 % string2);
  istart2 = MAX(fstart2, 0);
  if (istart2 > cl__length(string2)) {
    SIMPLE_ERROR(BF("start2 %d out of bounds for string %s") % istart2 % string2);
  }
  iend2 = MIN(end2.nilp() ? cl__length(string2) : unbox_fixnum(gc::As<Fixnum_sp>(end2)), cl__length(string2));
}

#define TEMPLATE_SINGLE_STRING_DISPATCHER(_string_,_function_,istart,iend) \
  if (gc::IsA<SimpleString_sp>(_string_)) {				\
    if (gc::IsA<SimpleBaseString_sp>(_string_)) { \
      auto sbcs2 = gc::As_unsafe<SimpleBaseString_sp>(_string_); \
      return _function_(*sbcs2,istart,iend); \
    } else {							\
      auto scs2 = gc::As_unsafe<SimpleCharacterString_sp>(_string_); \
      return _function_(*scs2,istart,iend); \
    } \
  } else { \
    if (gc::IsA<Str8Ns_sp>(_string_)) { \
      auto ns82 = gc::As_unsafe<Str8Ns_sp>(_string_); \
      return _function_(*ns82,istart,iend); \
    } else { \
      auto nsw2 = gc::As_unsafe<StrWNs_sp>(_string_); \
      return _function_(*nsw2,istart,iend); \
    } \
  }

#define TEMPLATE_HALF_STRING_DISPATCHER(_this_,_string2_,_function_,istart1,iend1,istart2,iend2) \
  if (gc::IsA<SimpleString_sp>(_string2_)) {				\
    if (gc::IsA<SimpleBaseString_sp>(_string2_)) { \
      auto sbcs2 = gc::As_unsafe<SimpleBaseString_sp>(_string2_); \
      return _function_(*_this_,*sbcs2,istart1,iend1,istart2,iend2); \
    } else {							\
      auto scs2 = gc::As_unsafe<SimpleCharacterString_sp>(_string2_); \
      return _function_(*_this_,*scs2,istart1,iend1,istart2,iend2); \
    } \
  } else { \
    if (gc::IsA<Str8Ns_sp>(_string2_)) { \
      auto ns82 = gc::As_unsafe<Str8Ns_sp>(_string2_); \
      return _function_(*_this_,*ns82,istart1,iend1,istart2,iend2); \
    } else { \
      auto nsw2 = gc::As_unsafe<StrWNs_sp>(_string2_); \
      return _function_(*_this_,*nsw2,istart1,iend1,istart2,iend2); \
    } \
  }

#define TEMPLATE_STRING_DISPATCHER(_string1_,_string2_,_function_,istart1,iend1,istart2,iend2) \
if (gc::IsA<SimpleString_sp>(_string1_) ) {			    \
  if (gc::IsA<SimpleString_sp>(_string2_)) {				\
    if (gc::IsA<SimpleBaseString_sp>(_string1_)) { \
      auto sbcs1 =  gc::As_unsafe<SimpleBaseString_sp>(_string1_); \
      if (gc::IsA<SimpleBaseString_sp>(_string2_)) { \
        auto sbcs2 = gc::As_unsafe<SimpleBaseString_sp>(_string2_); \
        return _function_(*sbcs1,*sbcs2,istart1,iend1,istart2,iend2); \
      } else {							\
        auto scs2 = gc::As_unsafe<SimpleCharacterString_sp>(_string2_); \
        return _function_(*sbcs1,*scs2,istart1,iend1,istart2,iend2); \
      } \
    } else { \
      auto scs1 = gc::As_unsafe<SimpleCharacterString_sp>(_string1_); \
      if (gc::IsA<SimpleBaseString_sp>(_string2_)) { \
        auto sbcs2 = gc::As_unsafe<SimpleBaseString_sp>(_string2_); \
        return _function_(*scs1,*sbcs2,istart1,iend1,istart2,iend2); \
      } else { \
        auto scs2 = gc::As_unsafe<SimpleCharacterString_sp>(_string2_); \
        return _function_(*scs1,*scs2,istart1,iend1,istart2,iend2); \
      } \
    } \
  } else { \
    if (gc::IsA<SimpleBaseString_sp>(_string1_)) { \
      auto sbcs1 = gc::As_unsafe<SimpleBaseString_sp>(_string1_); \
      if (gc::IsA<Str8Ns_sp>(_string2_)) { \
        auto ns82 = gc::As_unsafe<Str8Ns_sp>(_string2_); \
        return _function_(*sbcs1,*ns82,istart1,iend1,istart2,iend2);	\
      } else {							\
        auto nsw2 = gc::As_unsafe<StrWNs_sp>(_string2_); \
        return _function_(*sbcs1,*nsw2,istart1,iend1,istart2,iend2);	\
      }								\
    } else {								\
      auto scs1 = gc::As_unsafe<SimpleCharacterString_sp>(_string1_); \
      if (gc::IsA<Str8Ns_sp>(_string2_)) { \
        auto ns82 = gc::As_unsafe<Str8Ns_sp>(_string2_); \
        return _function_(*scs1,*ns82,istart1,iend1,istart2,iend2);	\
      } else {							\
        auto nsw2 = gc::As_unsafe<StrWNs_sp>(_string2_); \
        return _function_(*scs1,*nsw2,istart1,iend1,istart2,iend2);	\
      }								\
    }									\
  }									\
 } else { /* _string1_ is a StrNs_sp */  				    \
  if (gc::IsA<SimpleString_sp>(_string2_)) {				\
    /* _string2_ is a SimpleString_sp */ \
    if (gc::IsA<Str8Ns_sp>(_string1_)) { \
      auto ns81 = gc::As_unsafe<Str8Ns_sp>(_string1_); \
      if (gc::IsA<SimpleBaseString_sp>(_string2_)) { \
        auto sbcs2 = gc::As_unsafe<SimpleBaseString_sp>(_string2_); \
        return _function_(*ns81,*sbcs2,istart1,iend1,istart2,iend2); \
      } else { \
        auto scs2 = gc::As_unsafe<SimpleCharacterString_sp>(_string2_); \
        return _function_(*ns81,*scs2,istart1,iend1,istart2,iend2); \
      } \
    } else { \
      auto nsw1 = gc::As_unsafe<StrWNs_sp>(_string1_); \
      if (gc::IsA<SimpleBaseString_sp>(_string2_)) { \
        auto sbcs2 = gc::As_unsafe<SimpleBaseString_sp>(_string2_); \
        return _function_(*nsw1,*sbcs2,istart1,iend1,istart2,iend2); \
      } else { \
        auto scs2 = gc::As_unsafe<SimpleCharacterString_sp>(_string2_); \
        return _function_(*nsw1,*scs2,istart1,iend1,istart2,iend2); \
      } \
    } \
  } else { \
    if (gc::IsA<Str8Ns_sp>(_string1_)) { \
      auto ns81 = gc::As_unsafe<Str8Ns_sp>(_string1_); \
      if (gc::IsA<Str8Ns_sp>(_string2_)) { \
        auto ns82 = gc::As_unsafe<Str8Ns_sp>(_string2_); \
        return _function_(*ns81,*ns82,istart1,iend1,istart2,iend2); \
      } else { \
        auto nsw2 = gc::As_unsafe<StrWNs_sp>(_string2_); \
        return _function_(*ns81,*nsw2,istart1,iend1,istart2,iend2); \
      } \
    } else { \
      auto nsw1 = gc::As_unsafe<StrWNs_sp>(_string1_); \
      if (gc::IsA<Str8Ns_sp>(_string2_)) { \
        auto ns82 = gc::As_unsafe<Str8Ns_sp>(_string2_); \
        return _function_(*nsw1,*ns82,istart1,iend1,istart2,iend2); \
      } else { \
        auto nsw2 = gc::As_unsafe<StrWNs_sp>(_string2_); \
        return _function_(*nsw1,*nsw2,istart1,iend1,istart2,iend2); \
      } \
    } \
  } \
 } \
 SIMPLE_ERROR(BF("Illegal combination of string arguments in TEMPLATE_STRING_DISPATCHER"));





CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_EQ_");
CL_DEFUN T_sp cl__string_EQ_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_EQ_,istart1,iend1,istart2,iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_NE_");
CL_DEFUN T_mv cl__string_NE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_NE_, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_LT_");
CL_DEFUN T_mv cl__string_LT_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_LT_, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_GT_");
CL_DEFUN T_mv cl__string_GT_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_GT_, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_LE_");
CL_DEFUN T_mv cl__string_LE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_LE_, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_GE_");
CL_DEFUN T_mv cl__string_GE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_GE_, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_equal");
CL_DEFUN T_sp cl__string_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_equal, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_not_equal");
CL_DEFUN T_mv cl__string_not_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_not_equal, istart1, iend1, istart2, iend2);
}
CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_lessp");
CL_DEFUN T_mv cl__string_lessp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_lessp, istart1, iend1, istart2, iend2);
}
CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_greaterp");
CL_DEFUN T_mv cl__string_greaterp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_greaterp, istart1, iend1, istart2, iend2);
}
CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_not_greaterp");
CL_DEFUN T_mv cl__string_not_greaterp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_not_greaterp, istart1, iend1, istart2, iend2);
}
CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_not_lessp");
CL_DEFUN T_mv cl__string_not_lessp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_not_lessp, istart1, iend1, istart2, iend2);
}


// ------------------------------------------------------------
//
//



CL_LAMBDA("size &key initial-element (element-type 'character)");
CL_DECLARE();
CL_DOCSTRING("See CLHS: make_string");
CL_DEFUN T_sp cl__make_string(Fixnum_sp size, T_sp initial_element, T_sp element_type) {
  if (initial_element.nilp()) {
    initial_element = clasp_make_character(' ');
  }
  if (!initial_element.characterp()) {
    TYPE_ERROR(initial_element,cl::_sym_character);
  }
  if (!size.fixnump()) {
    TYPE_ERROR(size,cl::_sym_fixnum);
  }
  Fixnum sz = size.unsafe_fixnum();
  if (sz < 0 ) {
    SIMPLE_ERROR(BF("Size must be >= 0"));
  }
  claspCharacter initial_element_cc = initial_element.unsafe_character();
  if ( element_type == cl::_sym_base_char ) {
    if (!clasp_base_char_p(initial_element_cc)) {
      TYPE_ERROR(initial_element,cl::_sym_base_char);
    }
    SimpleBaseString_sp s = SimpleBaseString_O::make(size.unsafe_fixnum(),initial_element.unsafe_character(), true);
    return s;
  }
  SimpleCharacterString_sp s = SimpleCharacterString_O::make(size.unsafe_fixnum(),initial_element.unsafe_character(), true);
  return s;
}

SYMBOL_EXPORT_SC_(ClPkg,simple_string);

CL_LAMBDA(str index);
CL_DECLARE();
CL_DOCSTRING("CLHS schar");
CL_DEFUN Character_sp cl__schar(AbstractSimpleVector_sp str, size_t idx) {
  if (SimpleBaseString_sp sb = str.asOrNull<SimpleBaseString_O>()) {
    return clasp_make_character((*sb)[idx]);
  } else if (SimpleCharacterString_sp sc = str.asOrNull<SimpleCharacterString_O>()) {
    return clasp_make_character((*sc)[idx]);
  }
  TYPE_ERROR(str,cl::_sym_simple_string);
}


CL_LAMBDA(str idx);
CL_DOCSTRING("Common lisp char");
CL_DEFUN Character_sp cl__char(String_sp str, size_t idx) {
/* Return the character at idx - ignore fill pointers */
  if ( SimpleBaseString_sp sb = str.asOrNull<SimpleBaseString_O>() ) {
    return clasp_make_character((*sb)[idx]);
  } else if (Str8Ns_sp s8 = str.asOrNull<Str8Ns_O>() ) {
    return clasp_make_character((*s8)[idx]);
  } else if (SimpleCharacterString_sp sc = str.asOrNull<SimpleCharacterString_O>() ) {
    return clasp_make_character((*sc)[idx]);
  } else if (StrWNs_sp sw = str.asOrNull<StrWNs_O>() ) {
    return clasp_make_character((*sw)[idx]);
  }
  TYPE_ERROR(str,cl::_sym_string);
};

CL_LISPIFY_NAME("cl:char")
CL_LAMBDA(c str index);
CL_DECLARE();
CL_DOCSTRING("CLHS (setf char)");
CL_DEFUN_SETF Character_sp core__char_set(Character_sp c, String_sp str, size_t idx) {
  if ( SimpleBaseString_sp sb = str.asOrNull<SimpleBaseString_O>() ) {
    (*sb)[idx] = c.unsafe_character();
  } else if (Str8Ns_sp s8 = str.asOrNull<Str8Ns_O>() ) {
    (*s8)[idx] = c.unsafe_character();
  } else if (SimpleCharacterString_sp sc = str.asOrNull<SimpleCharacterString_O>() ) {
    (*sc)[idx] = c.unsafe_character();
  } else if (StrWNs_sp sw = str.asOrNull<StrWNs_O>() ) {
    (*sw)[idx] = c.unsafe_character();
  } else {
    TYPE_ERROR(str,cl::_sym_string);
  }
  return c;
};

CL_LISPIFY_NAME("cl:schar");
CL_LAMBDA(str index c);
CL_DECLARE();
CL_DOCSTRING("CLHS (setf schar)");
CL_DEFUN_SETF Character_sp core__schar_set(Character_sp c, String_sp str, size_t idx) {
  str->rowMajorAset(idx,c);
  return c;
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

cl_index fsmInteger(mpz_class &result, cl_index &numDigits, bool &sawJunk, String_sp str, cl_index istart, cl_index iend, bool junkAllowed, cl_index radix) {
  IntegerFSMState state = iinit;
  cl_index sign = 1;
  result = 0;
  numDigits = 0;
  cl_index cur = istart;
  while (1) {
    claspCharacter c = clasp_as_claspCharacter(gc::As_unsafe<Character_sp>(str->rowMajorAref(cur)));
    LOG(BF("fsmInteger str[%d] -> c = [%d/%c]") % cur  % c % c );
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
          LOG(BF("Hit junk at %d\n") % cur);
          state = ijunk;
          break;
        }
        result = result * GMP_LONG(radix) + GMP_LONG(idigit);
        ++numDigits;
        state = inum;
        break;
      }
      state = ijunk;
      break;
    }
    case inum: {
      if (isspace(c)) {
        // Optional leading and trailing whitespace[1] is ignored.
        // don't understand this but sbcl and ccl seem to agree, that if junkAllowed, than the parsing stop
        // once a space after the number is read
        if (junkAllowed) {
          state = ijunk; // itrailspace;
          break;
        }
        state = itrailspace;
        break;
      } else if (isalnum(c)) {
        cl_index idigit = fsmIntegerDigit(c, radix);
        if (idigit < 0 || idigit >= radix) {
          state = ijunk;
          break;
        }
        result = result * GMP_LONG(radix) + GMP_LONG(idigit);
        ++numDigits;
        state = inum;
        break;
      }
      state = ijunk;
      break;
    }
    case itrailspace: {
      if (!isspace(c)) {
        state = ijunk;
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
  LOG(BF("Returning with cur=%d") % cur);
  return cur;
};


CL_LAMBDA(string &key (start 0) end (radix 10) junk-allowed);
CL_DECLARE();
CL_DOCSTRING("parseInteger");
CL_DEFUN T_mv cl__parse_integer(String_sp str, Fixnum start, T_sp end, uint radix, T_sp junkAllowed) {
  Fixnum istart = std::max((Fixnum)0, start);
  Fixnum iend = cl__length(str);
  if (end.notnilp()) {
    iend = std::min(iend, unbox_fixnum(gc::As<Fixnum_sp>(end)));
  }
  mpz_class result;
  bool sawJunk = false;
  cl_index numDigits = 0;
  cl_index cur = fsmInteger(result, numDigits, sawJunk, str, istart, iend, junkAllowed.isTrue(), radix);
  if (junkAllowed.notnilp() || (cur >= iend) || !sawJunk) {
    // normal exit
    if (numDigits > 0) {
      Integer_sp iresult = Integer_O::create(result);
      LOG(BF("Returning parse-integer with result = %s  cur = %d") % _rep_(iresult) % cur );
      return (Values(iresult, make_fixnum(cur)));
    } else {
      //If junk-allowed is false, an error of type parse-error is signaled if substring does not consist entirely of the representation
      // of a signed integer, possibly surrounded on either side by whitespace[1] characters.
      // The first value returned is either the integer that was parsed, or else nil if no syntactically correct integer was seen but junk-allowed was true.
      if (junkAllowed.notnilp())
        return (Values(_Nil<T_O>(), make_fixnum(cur)));
      else PARSE_ERROR(SimpleBaseString_O::make("Could not parse integer from ~S"), Cons_O::create(str,_Nil<T_O>()));
    }
  }
  PARSE_ERROR(SimpleBaseString_O::make("Could not parse integer from ~S"), Cons_O::create(str,_Nil<T_O>()));
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

// ------------------------------------------------------------
//
// Class SimpleBaseString_O
//

bool SimpleBaseString_O::equal(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (!cl__stringp(other)) return false;
  String_sp sother = gc::As_unsafe<String_sp>(other);
  TEMPLATE_HALF_STRING_DISPATCHER(this,sother,template_string_EQ_equal,0,this->length(),0,sother->length());
};

bool SimpleBaseString_O::equalp(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (cl__stringp(other)) {
    String_sp sother = gc::As_unsafe<String_sp>(other);
    TEMPLATE_HALF_STRING_DISPATCHER(this,sother,template_string_equalp_bool,0,this->length(),0,sother->length());
  } else {
    return this->AbstractSimpleVector_O::equalp(other);
  }
}


// ------------------------------------------------------------
//
// Class SimpleCharacterString_O
//

bool SimpleCharacterString_O::equal(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (gc::IsA<SimpleString_sp>(other)) {
    if (gc::IsA<SimpleBaseString_sp>(other)) {
      auto so = gc::As_unsafe<SimpleBaseString_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    } else {
      auto so = gc::As_unsafe<SimpleCharacterString_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    }
  } else {
    if (gc::IsA<Str8Ns_sp>(other)) {
      auto so = gc::As_unsafe<Str8Ns_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    } else if (gc::IsA<StrWNs_sp>(other)) {
      auto so = gc::As_unsafe<StrWNs_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    }
  }
  return false;
};

bool SimpleCharacterString_O::equalp(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (cl__stringp(other)) {
    String_sp sother = gc::As_unsafe<String_sp>(other);
    TEMPLATE_HALF_STRING_DISPATCHER(this,sother,template_string_equalp_bool,0,this->length(),0,sother->length());
  } else {
    return this->AbstractSimpleVector_O::equalp(other);
  }
}

std::string SimpleCharacterString_O::get_std_string() const {
  std::string sout(this->length(),' ');
  for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) sout[i] = (*this)[i];
  return sout;
}

std::string SimpleCharacterString_O::__repr__() const {
  return this->get_std_string();
}

// ------------------------------------------------------------
//
// Class Str8Ns
//

bool Str8Ns_O::equal(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (gc::IsA<SimpleString_sp>(other)) {
    if (gc::IsA<SimpleBaseString_sp>(other)) {
      auto so = gc::As_unsafe<SimpleBaseString_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    } else {
      auto so = gc::As_unsafe<SimpleCharacterString_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    }
  } else {
    if (gc::IsA<Str8Ns_sp>(other)) {
      auto so = gc::As_unsafe<Str8Ns_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    } else if (gc::IsA<StrWNs_sp>(other)){
      auto so = gc::As_unsafe<StrWNs_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    }
  }
  return false;
};

bool Str8Ns_O::equalp(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (cl__stringp(other)) {
    String_sp sother = gc::As_unsafe<String_sp>(other);
    TEMPLATE_HALF_STRING_DISPATCHER(this,sother,template_string_equalp_bool,0,this->length(),0,sother->length());
  } else {
    return this-> MDArray_O::equalp(other);
  }
}

// Creators - depreciate these once the new array stuff is working better
Str8Ns_sp Str8Ns_O::create(const string& nm) {
  auto ss = SimpleBaseString_O::make(nm.size(),'\0',true,nm.size(),(const claspChar*)nm.c_str());
  return Str8Ns_O::make(nm.size(),'\0',false,_Nil<T_O>(),ss,false,0);
}

Str8Ns_sp Str8Ns_O::create(const char* nm,size_t len) {
  SimpleBaseString_sp ss = SimpleBaseString_O::make(len,'\0',true,len,(const claspChar*)nm);
  return Str8Ns_O::make(len,'\0',false,_Nil<T_O>(),ss,false,0);
}

Str8Ns_sp Str8Ns_O::create(const char* nm) {
  size_t len = strlen(nm);
  return Str8Ns_O::create(nm,len);
}

Str8Ns_sp Str8Ns_O::create(size_t len) {
  return Str8Ns_O::make(len,'\0',true,_Nil<T_O>(),_Nil<T_O>(),false,0);
}

Str8Ns_sp Str8Ns_O::create(Str8Ns_sp other) {
  size_t len = other->length();
  SimpleBaseString_sp ss = SimpleBaseString_O::make(len,'\0',true,len,&(*other)[0]);
  return Str8Ns_O::make(len,'\0',false,_Nil<T_O>(),ss,false,0);
}

SimpleString_sp Str8Ns_O::asMinimalSimpleString() const {
  SimpleBaseString_sp str8 = SimpleBaseString_O::make(this->length());
  str8->unsafe_setf_subseq(0,this->length(),this->asSmartPtr());
  return str8;
}



// ------------------------------------------------------------
//
// Class StrWNs
//

bool StrWNs_O::all_base_char_p() const {
  bool extended=false;
  for (size_t i(0),iEnd(this->length()); i<iEnd;++i) {
    if (!clasp_base_char_p((*this)[i])) extended = true;
  }
  return !extended;
}

bool StrWNs_O::equal(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (gc::IsA<SimpleString_sp>(other)) {
    if (gc::IsA<SimpleBaseString_sp>(other)) {
      auto so = gc::As_unsafe<SimpleBaseString_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    } else {
      auto so = gc::As_unsafe<SimpleCharacterString_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    }
  } else {
    if (gc::IsA<Str8Ns_sp>(other)) {
      auto so = gc::As_unsafe<Str8Ns_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    } else if (gc::IsA<StrWNs_sp>(other)){
      auto so = gc::As_unsafe<StrWNs_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    }
  }
  return false;
};

bool StrWNs_O::equalp(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (cl__stringp(other)) {
    String_sp sother = gc::As_unsafe<String_sp>(other);
    TEMPLATE_HALF_STRING_DISPATCHER(this,sother,template_string_equalp_bool,0,this->length(),0,sother->length());
  } else {
    return this->MDArray_O::equalp(other);
  }
}

std::string StrWNs_O::get_std_string() const {
  std::string sout(this->length(),' ');
  for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) sout[i] = (*this)[i];
  return sout;
}

std::string StrWNs_O::__repr__() const {
  return this->get_std_string();
}

SimpleString_sp StrWNs_O::asMinimalSimpleString() const {
  if (this->all_base_char_p()) {
    SimpleBaseString_sp str8 = SimpleBaseString_O::make(this->length());
    str8->unsafe_setf_subseq(0,this->length(),this->asSmartPtr());
    return str8;
  } else {
    SimpleCharacterString_sp strw = SimpleCharacterString_O::make(this->length());
    strw->unsafe_setf_subseq(0,this->length(),this->asSmartPtr());
    return strw;
  }
}

SYMBOL_EXPORT_SC_(ClPkg,vectorPushExtend);

}; // namespace core

namespace core {
void StringPushSubString(String_sp buffer, String_sp str, size_t start, size_t end) {
  for ( ; start<end; ++start ) {
    buffer->vectorPushExtend(cl__char(str, start));
  }
}

void StringPushString(String_sp buffer, String_sp other) {
  StringPushSubString(buffer, other, 0, cl__length(other));
}

void StringPushStringCharStar(String_sp buffer, const char *cPtr) {
  while (*cPtr) {
    buffer->vectorPushExtend(clasp_make_character(*cPtr), 64);
    ++cPtr;
  }
}

string string_get_std_string(String_sp str) { return str->get_std_string(); };
string string_get_std_string(T_sp str) {
  if (str.nilp()) {
    SIMPLE_ERROR(BF("Could not convert nil to Str"));
  };
  return gc::As<String_sp>(str)->get_std_string();
};
SimpleBaseString_sp str_create(const string &str) { return SimpleBaseString_O::make(str); };
SimpleBaseString_sp str_create(const char *str) { return SimpleBaseString_O::make(std::string(str)); };

CL_LAMBDA(core:&va-rest args);
CL_LISPIFY_NAME(base_string_concatenate);
CL_DEFUN T_sp core__base_string_concatenate(VaList_sp vargs) {
  size_t nargs = vargs->remaining_nargs();
  stringstream ss;
  for (size_t i(0); i < nargs; ++i) {
    T_sp csp = vargs->next_arg();
    String_sp ssp = coerce::stringDesignator(csp);
    ss << ssp->get_std_string();
  }
  return SimpleBaseString_O::make(ss.str());
};

template <typename T1,typename T2>
T_sp template_search_string(const T1& sub, const T2& outer, size_t sub_start, size_t sub_end, size_t outer_start, size_t outer_end)
{
  // The std::search convention is reversed -->  std::search(outer,sub,...)
  const typename T2::simple_element_type* startp = &outer[0];
  const typename T2::simple_element_type* cps = &outer[outer_start];
  const typename T2::simple_element_type* cpe = &outer[outer_end];
  const typename T1::simple_element_type* s_cps = &sub[sub_start];
  const typename T1::simple_element_type* s_cpe = &sub[sub_end];
  const typename T2::simple_element_type* pos = std::search(cps,cpe,s_cps,s_cpe);
  if (pos == cpe ) return _Nil<T_O>();
  // this should return the absolute position starting from 0, not relative to outer_start
  //now that I understood this in pointer arithmethic, compare to the beginning of the string, e.g. index 0
  return clasp_make_fixnum(pos-startp);
}

SYMBOL_EXPORT_SC_(CorePkg,search_string);
CL_LAMBDA(sub sub_start sub_end outer outer_start outer_end);
CL_DOCSTRING("search for the first occurance of sub in outer");
CL_DEFUN T_sp core__search_string(String_sp sub, size_t sub_start, T_sp sub_end, String_sp outer, size_t outer_start, T_sp outer_end) {
  size_t_pair psub = sequenceStartEnd(_sym_search_string,sub->length(),sub_start,sub_end);
  size_t_pair pouter = sequenceStartEnd(_sym_search_string,outer->length(),outer_start,outer_end);
  TEMPLATE_STRING_DISPATCHER(sub,outer,template_search_string,psub.start,psub.end,pouter.start,pouter.end);
};


CL_LISPIFY_NAME("core:split");
CL_DEFUN List_sp core__split(const string& all, const string &chars) {
  vector<string> parts = split(all, chars);
  T_sp first = _Nil<T_O>();
  T_sp* cur = &first;
  for (vector<string>::iterator it = parts.begin(); it != parts.end(); it++) {
    Cons_sp cons = Cons_O::create(Str_O::create(*it), _Nil<T_O>());
    *cur = cons;
    cur = &(cons->_Cdr);
  }
  return first;
}

CL_DEFUN T_sp core__copy_to_simple_base_string(T_sp x)
{
 AGAIN:
  if (x.characterp()) {
    x = cl__string(x);
    goto AGAIN;
  } else if (gc::IsA<Symbol_sp>(x)) {
    x = gc::As_unsafe<Symbol_sp>(x)->symbolName();
    goto AGAIN;
  }
#ifdef CLASP_UNICODE
  unlikely_if (gc::IsA<StrWNs_sp>(x)) {
    StrWNs_sp wx = gc::As_unsafe<StrWNs_sp>(x);
    AbstractSimpleVector_sp bsv;
    size_t start, end;
    wx->asAbstractSimpleVectorRange(bsv,start,end);
    SimpleCharacterString_sp swx = gc::As_unsafe<SimpleCharacterString_sp>(bsv);
    SimpleBaseString_sp y = SimpleBaseString_O::make(wx->length());
    for (size_t index(0); index < wx->length(); ++index ) {
      claspCharacter c = (*swx)[index+start];
      if (!clasp_base_char_p(c)) {
        SIMPLE_ERROR(BF("Cannot coerce string %s to a base-string") % _rep_(x));
      }
      (*y)[index] = c;
    }
    return y;
  }
  unlikely_if (gc::IsA<SimpleCharacterString_sp>(x)) {
    SimpleCharacterString_sp sx = gc::As_unsafe<SimpleCharacterString_sp>(x);
    SimpleBaseString_sp y = SimpleBaseString_O::make(sx->length());
    for (size_t index(0); index < sx->length(); ++index ) {
      claspCharacter c = (*sx)[index];
      if (!clasp_base_char_p(c)) {
        SIMPLE_ERROR(BF("Cannot coerce string %s to a base-string") % _rep_(x));
      }
      (*y)[index] = c;
    }
    return y;
  }
#endif
  if (core__base_string_p(x)) {
    String_sp sx = gc::As_unsafe<String_sp>(x);
    AbstractSimpleVector_sp bsv;
    size_t start, end;
    sx->asAbstractSimpleVectorRange(bsv,start,end);
    SimpleBaseString_sp swx = gc::As_unsafe<SimpleBaseString_sp>(bsv);
    SimpleBaseString_sp y = SimpleBaseString_O::make(sx->length());
    memcpy(&(*y)[0],&(*swx)[start],sx->length());
    return y;
  }
  SIMPLE_ERROR(BF("Could not copy %s to simple-base-string") % _rep_(x));
}


template <typename T1>
bool template_fits_in_base_string(const T1& sub, size_t start, size_t end)
{
  // The std::search convention is reversed -->  std::search(outer,sub,...)
  const typename T1::simple_element_type* s_cps = (typename T1::simple_element_type*)sub.rowMajorAddressOfElement_(start); // &sub[start];
  const typename T1::simple_element_type* s_cpe = (typename T1::simple_element_type*)sub.rowMajorAddressOfElement_(end); // &sub[end];
  for ( ; s_cps != s_cpe; ++s_cps ) {
    if ( !clasp_base_char_p(*s_cps) ) {
      return false;
    }
  }
  return true;
}

CL_DEFUN bool core__fits_in_base_string(T_sp tstr) {
  String_sp str = gc::As<String_sp>(tstr);
  TEMPLATE_SINGLE_STRING_DISPATCHER(str,template_fits_in_base_string,0,str->length());
}

}; // namespace core
