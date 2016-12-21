/*
    File: str.cc
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
#include <algorithm>
//#include "clasp_gmpxx.h"
#include <ctype.h>
#include <clasp/gctools/telemetry.h>
#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/character.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/bignum.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/str.h>
#include <clasp/core/print.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/wrappers.h>

namespace core {


string str_get(Str_sp str) { return gc::As<Str_sp>(str)->get(); };
string str_get(T_sp str) {
  if (str.nilp()) {
    SIMPLE_ERROR(BF("Could not convert nil to Str"));
  };
  return gc::As<Str_sp>(str)->get();
};
T_sp str_create(const string &str) { return Str_O::create(str); };
T_sp str_create(const char *str) { return Str_O::create(str); };

CL_LAMBDA(str1 start1 end1 str2 start2 end2);
CL_DECLARE();
CL_DOCSTRING("search for the first occurance of str1 in str2");
CL_DEFUN T_sp core__search_string(Str_sp str1, Fixnum_sp start1, T_sp end1, Str_sp str2, Fixnum_sp start2, T_sp end2) {
  ASSERT(start1.fixnump());
  ASSERT(start2.fixnump());
  // This needs to be generalized to String_sp
  Str_sp s1 = str1->subseq(start1.unsafe_fixnum(), end1);
  Str_sp s2 = str2->subseq(start2.unsafe_fixnum(), end2);
  Str_O::element_type* first = &(*s2)[0];
  Str_O::element_type* last = &(*s2)[s2->length()];
  Str_O::element_type* s_first = &(*s1)[0];
  Str_O::element_type* s_last = &(*s1)[s1->length()];
  Str_O::element_type* found = std::search(first,last,s_first,s_last);
  if (found == last) {
    return _Nil<core::T_O>();
  }
  return clasp_make_fixnum(found-first);
};


Str_sp Str_O::create_with_fill_pointer(char initial_element, size_t dimension, cl_index fill_ptr, bool adjustable) {
  GC_ALLOCATE_VARIADIC(Str_O, str, dimension, clasp_make_fixnum(fill_ptr), _Nil<T_O>(), 0 );
  str->_String = string(dimension, initial_element);
  return str;
}


Str_sp Str_O::create(Str_sp val) {
  GC_ALLOCATE_VARIADIC(Str_O, v, val->length() ,clasp_make_fixnum(val->fillPointer()),_Nil<T_O>(),0);
  str_type temp((char*)&((*val)[0]),val->length());
  v->_String.swap(temp);
  return v;
}

Str_sp Str_O::create(const boost::format &nm) {
  string s = nm.str();
  return Str_O::create(s);
};

Str_sp Str_O::create(const string &nm) {
  GC_ALLOCATE_VARIADIC(Str_O, v, nm.size(),_Nil<T_O>(),_Nil<T_O>(),0);
  str_type temp(nm);
  v->_String.swap(temp);
  return v;
};

Str_sp Str_O::create(const char *nm) {
  cl_index len = strlen(nm);
  GC_ALLOCATE_VARIADIC(Str_O, v,len,_Nil<T_O>(),_Nil<T_O>(),0);
  //	printf("%s:%d Str_O::create(const char* nm) @ %p nm = %s\n", __FILE__, __LINE__, v.raw_(), nm );
  str_type temp(nm,len);
  v->_String.swap(temp);
  return v;
};

Str_sp Str_O::create(const char *nm, cl_index numChars) {
  GC_ALLOCATE_VARIADIC(Str_O, v,numChars,_Nil<T_O>(),_Nil<T_O>(),0);
  //	printf("%s:%d Str_O::create(const char* nm) @ %p nm = %s\n", __FILE__, __LINE__, v.raw_(), nm );
  str_type temp(nm,numChars);
  v->_String.swap(temp);
  return v;
};

Str_sp Str_O::create(cl_index dim, Str_sp displacedTo, cl_index displacedIndexOffset )
{
  GC_ALLOCATE_VARIADIC(Str_O, v,dim,_Nil<T_O>(),displacedTo,displacedIndexOffset);
  return v;
};
  
Str_sp Str_O::create(size_t numChars) {
  GC_ALLOCATE_VARIADIC(Str_O, v,numChars,_Nil<T_O>(),_Nil<T_O>(),0);
  //	printf("%s:%d Str_O::create(const char* nm) @ %p nm = %s\n", __FILE__, __LINE__, v.raw_(), nm );
  str_type temp(numChars);
  v->_String.swap(temp);
  return v;
};

Bignum Str_O::stringToBignum(const char *str) {
  Bignum bn = 0;
  for (const unsigned char *cp = (const unsigned char *)str; *cp; ++cp) {
    bn = (bn << 7) | ((*cp) & 0x7f);
  }
  return bn;
}

CL_LAMBDA(&va-rest args);
CL_LISPIFY_NAME(base_string_concatenate);
CL_DEFUN T_sp core__base_string_concatenate(T_sp args) {
  if (!args.valistp()) {
    SIMPLE_ERROR(BF("arg must be valist"));
  }
  VaList_sp vargs = gctools::As<VaList_sp>(args);
  size_t nargs = vargs->remaining_nargs();
  stringstream ss;
  for (size_t i(0); i < nargs; ++i) {
    T_sp csp = vargs->next_arg();
    Str_sp ssp = coerce::stringDesignator(csp);
    ss << ssp->c_str();
  }
  return Str_O::create(ss.str());
};

Str_sp Str_O::create(char initial_element, cl_index dimension) {
  GC_ALLOCATE_VARIADIC(Str_O, str,dimension,_Nil<T_O>(),_Nil<T_O>(),0);
  str->_String = string(dimension, initial_element);
  return str;
}

#if defined(OLD_SERIALIZE)
void Str_O::serialize(serialize::SNode node) {
  if (node->saving()) {
    // Do nothing
  } else {
    IMPLEMENT_ME();
  }
}
#endif

Rational_sp Str_O::parseInteger() {
  return Integer_O::create(this->get());
}

CL_LISPIFY_NAME("core:parse-real");
CL_DEFMETHOD DoubleFloat_sp Str_O::asReal() const {
  DoubleFloat_sp n;
  n = DoubleFloat_O::create(atof(this->get().c_str()));
  return n;
}

char& Str_O::operator[](cl_index i) {
  if (this->_DisplacedTo) {
    return (*this->_DisplacedTo)[this->_DisplacedIndexOffset + i];
  }
  return this->_String[i];
}

const char& Str_O::operator[](cl_index i) const {
  if (this->_DisplacedTo) {
    return (*this->_DisplacedTo)[this->_DisplacedIndexOffset + i];
  }
  return this->_String[i];
}

T_sp Str_O::setf_elt(cl_index index, T_sp val) {
  ASSERTF(index >= 0 && index < this->size(), BF("Index out of range for string: %d") % index);
  char ch = clasp_as_char(val);
  (*this)[index] = ch;
  return val;
}

T_sp Str_O::aref(VaList_sp indices) const {
  ASSERT(indices->remaining_nargs() == 1);
  core::T_sp arg0 = indices->next_arg();
  ASSERT(arg0.fixnump());
  cl_index index = arg0.unsafe_fixnum();
  if (index >= this->size()) {
    SIMPLE_ERROR(BF("Index %d out of bounds - must be [0,%d)") % index % this->size());
  }
  return clasp_make_character((*this)[index]);
}

T_sp Str_O::setf_aref(List_sp indices_val) {
  ASSERTF(cl__length(indices_val) == 2, BF("Illegal index/val for setf_aref of string: %s") % _rep_(indices_val));
  cl_index index = unbox_fixnum(gc::As<Fixnum_sp>(oCar(indices_val)));
  T_sp val = oCadr(indices_val);
  ASSERT(val.characterp());
  (*this)[index] = val.unsafe_character();
  return val;
}

CL_LISPIFY_NAME("core:countOccurances");
CL_DEFMETHOD uint Str_O::countOccurances(const string &chars) {
  ASSERT_eq(chars.size(), 1);
  uint count = 0;
  for ( auto ch : chars ) {
    count += std::count(this->begin(),this->end(),ch);
  }
  return count;
}

CL_LISPIFY_NAME("core:left");
CL_DEFMETHOD Str_sp Str_O::left(gc::Fixnum num) const {
  return this->subseq(0,clasp_make_fixnum(num));
}

CL_LISPIFY_NAME("core:right");
CL_DEFMETHOD Str_sp Str_O::right(gc::Fixnum num) const {
  return this->subseq(this->length()-num,clasp_make_fixnum(num));
}

CL_LISPIFY_NAME("core:string-find");
CL_DEFMETHOD T_sp Str_O::find(const string &substring, gc::Fixnum start) {
  cl_index res = this->get().find(substring, start);
  if (res != string::npos)
    return Integer_O::create((gc::Fixnum)res);
  return _Nil<T_O>();
}

CL_LISPIFY_NAME("core:splitAtWhiteSpace");
CL_DEFMETHOD List_sp Str_O::splitAtWhiteSpace() {
  string all = this->get();
  vector<string> parts = core::split(all, " \n\t");
  T_sp first = _Nil<T_O>();
  T_sp* cur = &first;
  for (vector<string>::iterator it = parts.begin(); it != parts.end(); it++) {
    Cons_sp cons = Cons_O::create(Str_O::create(*it), _Nil<T_O>());
    *cur = cons;
    cur = &(cons->_Cdr);
  }
  return first;
}


CL_LISPIFY_NAME("core:split");
CL_DEFMETHOD List_sp Str_O::split(const string &chars) {
  string all = this->get();
  vector<string> parts = core::split(all, chars);
  T_sp first = _Nil<T_O>();
  T_sp* cur = &first;
  for (vector<string>::iterator it = parts.begin(); it != parts.end(); it++) {
    Cons_sp cons = Cons_O::create(Str_O::create(*it), _Nil<T_O>());
    *cur = cons;
    cur = &(cons->_Cdr);
  }
  return first;
}

#if 0
bool Str_O::eql_(T_sp obj) const {

  if (Str_sp t = obj.asOrNull<Str_O>()) {
    return this->get() == t->get();
  }
  return this->Base::eql_(obj);
}
#endif

#if 0
bool Str_O::equal(T_sp obj) const {
  if (core__simple_string_p(obj)) {
    return this->eql_(obj);
  }
  return false;
}
#endif

void Str_O::sxhash_(HashGenerator &hg) const {
  if (hg.isFilling()) {
    Fixnum hash = 5381;
    Fixnum c;
    for (cl_index i(0), iEnd(cl__length(this->asSmartPtr())); i < iEnd; ++i) {
      c = (*this)[i];
      hash = ((hash << 5) + hash) + c;
    }
    hg.addPart(hash);
  }
}

string Str_O::__repr__() const {
  stringstream ss;
  ss << '"';
  for ( cl_index i(0), iEnd(this->length()); i<iEnd; ++i ) {
    char c = (*this)[i];
    if (c == '"') {
      ss << '\\' << '"';
    } else if (c == '\n') {
      ss << '\\' << 'n';
    } else {
      ss << c;
    }
  }
  ss << '"';
  return ss.str();
}


#ifndef USE_TEMPLATE_STRING_MATCHER
/*! bounding index designator range from 0 to the end of each string */
T_sp Str_O::string_EQ_(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const {
  
  const char *cp1 = &(*this)[start1];
  const char *cp2 = &(*string2)[start2];
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
T_sp Str_O::string_NE_(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const {
  const char *cp1 = &(*this)[start1];
  const char *cp2 = &(*string2)[start2];
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
T_sp Str_O::string_LT_(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const {
  _OF();
  const char *cp1 = &(*this)[start1];
  const char *cp2 = &(*string2)[start2];
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
  return make_fixnum((cl_index)(cp1 - &(*this)[0]));
END_STRING2:
RETURN_FALSE:
  return _Nil<T_O>();
}

/*! bounding index designator range from 0 to the end of each string */
T_sp Str_O::string_GT_(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const {
  _OF();
  const char *cp1 = &(*this)[start1];
  const char *cp2 = &(*string2)[start2];
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
  return make_fixnum((int)(cp1 - &(*this)[0]));
}

/*! bounding index designator range from 0 to the end of each string */
T_sp Str_O::string_LE_(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const {
  const char *cp1 = &(*this)[start1];
  const char *cp2 = &(*string2)[start2];
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
  return make_fixnum((int)(cp1 - &(*this)[0]));
}

/*! bounding index designator range from 0 to the end of each string */
T_sp Str_O::string_GE_(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const {
  const char *cp1 = &(*this)[start1];
  const char *cp2 = &(*string2)[start2];
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
  return make_fixnum((int)(cp1 - &(*this)[0]));
}

/*! bounding index designator range from 0 to the end of each string */
T_sp Str_O::string_equal(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const {
  const char *cp1 = &(*this)[start1];
  const char *cp2 = &(*string2)[start2];
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
T_sp Str_O::string_not_equal(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const {
  const char *cp1 = &(*this)[start1];
  const char *cp2 = &(*string2)[start2];
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
T_sp Str_O::string_lessp(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const {
  const char *cp1 = &(*this)[start1];
  const char *cp2 = &(*string2)[start2];
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
  return make_fixnum((int)(cp1 - &(*this)[0]));
END_STRING2:
RETURN_FALSE:
  return _Nil<T_O>();
}

/*! bounding index designator range from 0 to the end of each string */
T_sp Str_O::string_greaterp(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const {
  const char *cp1 = &(*this)[start1];
  const char *cp2 = &(*string2)[start2];
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
  return make_fixnum((int)(cp1 - &(*this)[0]));
}

/*! bounding index designator range from 0 to the end of each string */
T_sp Str_O::string_not_greaterp(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const {
  const char *cp1 = &(*this)[start1];
  const char *cp2 = &(*string2)[start2];
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
  return make_fixnum((int)(cp1 - &(*this)[0]));
}

/*! bounding index designator range from 0 to the end of each string */
T_sp Str_O::string_not_lessp(Str_sp string2, cl_index start1, cl_index end1, cl_index start2, cl_index end2) const {
  const char *cp1 = &(*this)[start1];
  const char *cp2 = &(*string2)[start2];
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
  return make_fixnum((int)(cp1 - &(*this)[0]));
}
#endif // USE_TEMPLATE_STRING_MATCHER



T_sp Str_O::subseq(cl_index start, T_sp end) const {
  coerce::inBoundsOrError(start,0,this->length());
  cl_index iend = coerce::coerceToEndInRangeOrError(end,start,this->length());
  size_t ilen = iend - start;
  LOG(BF("Creating str with length = %lu") % ilen);
  Str_sp news = Str_O::create(ilen);
  memcpy(&((*news)[0]),&((*this)[start]),ilen*sizeof(element_type));
  return news;
}

T_sp Str_O::setf_subseq(cl_index start, T_sp end, T_sp new_subseq) {
  coerce::inBoundsOrError(start,0,this->length());
  cl_index iend = coerce::coerceToEndInRangeOrError(end,start,this->length());
  Str_sp sfrom = gc::As<Str_sp>(new_subseq);
  cl_index ileft = iend - start;
  for (cl_index idest(start),ifrom(0); idest<iend; ++idest, ++ifrom) {
    (*this)[idest] = (*sfrom)[ifrom];
  }
  return new_subseq;
}

claspChar Str_O::schar(cl_index index) const {
  coerce::inBoundsBelowEndOrError(index,0,this->length());
  return (*this)[index];
}

claspChar Str_O::scharSet(cl_index index, claspChar c) {
  coerce::inBoundsBelowEndOrError(index,0,this->length());
  (*this)[index] = c;
  return c;
}

void Str_O::fillArrayWithElt(T_sp element, Fixnum_sp start, T_sp end) {
  char celement = clasp_as_char(gc::As<Character_sp>(element));
  cl_index istart = unbox_fixnum(start);
  cl_index last = this->size();
  cl_index iend = last;
  if (end.notnilp()) iend = unbox_fixnum(gc::As<Fixnum_sp>(end));
  unlikely_if (iend <istart) {
    SIMPLE_ERROR(BF("Illegal fill range start is %lu and end is %lu") % istart % iend);
  }
  unlikely_if (iend > last) {
    iend = last;
  }
  unlikely_if (istart < 0) {
    SIMPLE_ERROR(BF("Illegal value for start[%d] - must be between 0 and %d") % istart % iend);
  }
  for (cl_index i = istart; i < iend; i++) {
    (*this)[i] = celement;
  }
}

void *Str_O::addressOfBuffer() const {
  return (void *)&((*this)[0]);
}

void Str_O::fillPointerSet(T_sp fp) {
  if (fp.fixnump()) {
    cl_index ifp = fp.unsafe_fixnum();
    if (ifp >= 0 && ifp <= this->arrayTotalSize()) {
      this->_FillPointer = fp;
      return;
    }
  } else if (fp.nilp()) {
    this->_FillPointer = fp;
    return;
  }
  SIMPLE_ERROR(BF("Illegal fill-pointer %d - must be less than %d or nil") % fp % this->length());
}

char *Str_O::addressOfFillPointer() {
  unlikely_if (!this->_FillPointer.fixnump()) noFillPointerError();
  return static_cast<char *>(this->addressOfBuffer()) + this->_FillPointer.unsafe_fixnum();
}

void Str_O::incrementFillPointer(cl_index off) {
  unlikely_if (!this->_FillPointer.fixnump()) noFillPointerError();
  this->_FillPointer = clasp_make_fixnum(this->_FillPointer.unsafe_fixnum() + off);
}

void Str_O::fillInitialContents(T_sp seq) {
  if (Cons_sp cls = seq.asOrNull<Cons_O>()) {
    List_sp ls = cls;
    if (cl__length(seq) != this->size())
      goto ERROR;
    cl_index i = 0;
    for (auto cur : ls) {
      (*this)[i] = clasp_as_char(gc::As<Character_sp>(oCar(cur)));
      ++i;
    }
  } else if (Str_sp ss = seq.asOrNull<Str_O>()) {
    if (ss->length() != this->size())
      goto ERROR;
    for (cl_index i = 0; i < this->size(); ++i) {
      (*this)[i] = (*ss)[i];
    }
  } else if (Vector_sp vs = seq.asOrNull<Vector_O>()) {
    if (vs->length() != this->size())
      goto ERROR;
    for (cl_index i = 0; i < this->size(); ++i) {
      (*this)[i] = clasp_as_char(gc::As<Character_sp>(vs));
    }
  } else {
    SIMPLE_ERROR(BF("Illegal :INITIAL-CONTENTS"));
  }
  return;
ERROR:
  SIMPLE_ERROR(BF("There are %d elements in the :INITIAL-CONTENTS, but the %s length is %d") % cl__length(seq) % _rep_(cl__class_of(seq)->className()) % this->size());
}

void Str_O::__write__(T_sp stream) const {
  cl_index ndx;
  if (!clasp_print_escape() && !clasp_print_readably()) {
    for (ndx = 0; ndx < this->size(); ndx++) {
      clasp_write_char((*this)[ndx], stream);
    }
  } else {
    clasp_write_char('"', stream);
    for (ndx = 0; ndx < this->size(); ndx++) {
      char c = (*this)[ndx];
      if (c == '"' || c == '\\')
        clasp_write_char('\\', stream);
      clasp_write_char(c, stream);
    }
    clasp_write_char('"', stream);
  }
}

void Str_O::setSize(cl_index newSize) {
  ASSERT(!this->_DisplacedTo);
  if (newSize < 0) {
    SIMPLE_ERROR(BF("You can only set size to >= 0 - not %d") % newSize);
  }
  if (this->_String.size() == newSize) return;
  if (this->_String.size() < newSize) {
    this->_String.resize(newSize, ' ');
    this->_Dimension = newSize;
  } else {
    this->_String.resize(newSize);
    this->_Dimension = newSize;
    if ( this->_FillPointer.fixnump()) {
      if (this->_FillPointer.unsafe_fixnum() > this->_String.size()) {
        this->_FillPointer = clasp_make_fixnum(this->_String.size());
      }
    }
  }
}

void Str_O::ensureSpaceAfterFillPointer(cl_index size) {
  ASSERT(!this->_DisplacedTo);
  if (!this->_FillPointer.fixnump()) noFillPointerError();
  cl_index left = this->arrayTotalSize() - this->_FillPointer.unsafe_fixnum();
  if (left < size) {
    this->setSize((size-left)+this->arrayTotalSize());
  }
}


T_sp Str_O::vectorPush(T_sp newElement) {
  ASSERT(newElement.characterp());
  if (!this->_FillPointer.fixnump()) noFillPointerError();
  cl_index idx = this->_FillPointer.unsafe_fixnum();
  if (idx < this->_Dimension) {
    (*this)[idx] = newElement.unsafe_character();
    this->_FillPointer = clasp_make_fixnum(idx+1);
    return clasp_make_fixnum(idx);
  }
  return _Nil<T_O>();
}

Fixnum_sp Str_O::vectorPushExtend(T_sp newElement, cl_index extension) {
  unlikely_if (!this->_FillPointer.fixnump()) {
    SIMPLE_ERROR(BF("This string does not have a fill pointer"));
  }
  cl_index idx = this->_FillPointer.unsafe_fixnum();
  unlikely_if (idx >= this->arrayTotalSize() ) {
    if (extension <= 0) extension = 32;
  }
  cl_index new_size = this->arrayTotalSize()+extension;
  unlikely_if (!cl::_sym_adjust_array->boundP()) {
    this->setSize(new_size);
  } else {
    eval::funcall(cl::_sym_adjust_array,this->asSmartPtr(),clasp_make_fixnum(new_size),cl::_sym_fill_pointer,this->_FillPointer);
  }
  (*this)[idx] = clasp_as_character(newElement);
  this->_FillPointer = clasp_make_fixnum(idx+1);
  return make_fixnum(idx);
}

void Str_O::pushSubString(T_sp tstr, cl_index start, cl_index end) {
  Str_sp str = cl__string(tstr);
  while (start < end) {
    this->vectorPushExtend(clasp_make_character(cl__char(str, start)));
    start++;
  }
}

CL_LISPIFY_NAME(push-string);
CL_DEFMETHOD void Str_O::pushString(T_sp str) {
  this->pushSubString(str, 0, cl__length(str));
}

void Str_O::pushStringCharStar(const char *cPtr) {
  while (*cPtr) {
    this->vectorPushExtend(clasp_make_character(*cPtr), this->length());
    cPtr++;
  }
}




  SYMBOL_SC_(CorePkg, base_string_concatenate);





}; /* core */
