/*
    File: array.cc
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
//#define DEBUG_LEVEL_FULL

#include <clasp/core/common.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/environment.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/array.h>
#include <clasp/core/wrappers.h>


namespace core {

// -----------------------------------------------------------
//
// Utility functions
//
void noFillPointerError() {
  SIMPLE_ERROR(BF("This vector does not have a fill pointer"));
}


// ------------------------------------------------------------
//
// MDArray_O
//
//

// Constructor
MDArray_O::MDArray_O(size_t rank,
                     List_sp dimensions,
                     T_sp elementType,
                     T_sp fillPointer,
                     Array_sp displacedTo,
                     size_t displacedIndexOffset,
                     T_sp initial_element,
                     bool initial_element_supplied_p )
   : _DisplacedIndexOffset(displacedIndexOffset),
     _FillPointerP(false),
    _Dimensions(rank,0) {
   size_t arrayTotalSize = 1;
   size_t irank = 0;
   for ( auto cur : dimensions ) {
     T_sp tdim = oCar(cur);
     unlikely_if (!tdim.fixnump()) {
       TYPE_ERROR(tdim,cl::_sym_integer);
     }
     size_t dim = tdim.unsafe_fixnum();
     arrayTotalSize *= dim;
     unlikely_if (irank >= rank) {
       SIMPLE_ERROR(BF("Too many dimensions %s for array of rank %d") % _rep_(dimensions) % rank);
     }
     this->_Dimensions[++irank] = dim;
     ++irank;
   }
   if ( rank == 1 ) {
     if ( fillPointer.fixnump() ) {
       // FillPointer
       this->_FillPointerOrLengthOrDummy = fillPointer.unsafe_fixnum();
       this->_FillPointerP = true;
     } else {
       // Length
       this->_FillPointerOrLengthOrDummy = arrayTotalSize;
     }
   } else {
     // Dummy for MDArray
     this->_FillPointerOrLengthOrDummy =  0xDEADBEEF01234567;
   }
   this->_ArrayTotalSize = arrayTotalSize;
   if ( displacedTo.notnilp() ) {
     this->_Data = gc::As<Array_sp>(displacedTo);
     this->_DisplacedToP = true;
   } else {
     this->_Data = core__make_vector(elementType, arrayTotalSize,false,_Nil<T_O>(),_Nil<T_O>(),0,initial_element,true);
     this->_DisplacedIndexOffset = 0;
     this->_DisplacedToP = false;
   }
 }



// ----------------------------------------------------------------------
//


CL_LAMBDA(core::array);
CL_DECLARE();
CL_DOCSTRING("arrayDisplacement");
CL_DEFUN T_mv cl__array_displacement(Array_sp array) {
  return Values(array->displaced_to(),clasp_make_fixnum(array->displaced_index_offset()));
}

CL_LAMBDA(core::type &optional core::env);
CL_DECLARE();
CL_DOCSTRING("upgradedArrayElementType");
CL_DEFUN T_mv cl__upgraded_array_element_type(T_sp type, T_sp env) {
  IMPLEMENT_MEF(BF("Handle new types"));
  return Values(T_O::static_class);
};

CL_LAMBDA(dest destStart orig origStart len);
CL_DECLARE();
CL_DOCSTRING("copy_subarray");
CL_DEFUN void core__copy_subarray(Array_sp dest, Fixnum_sp destStart, Array_sp orig, Fixnum_sp origStart, Fixnum_sp len) {
  // TODO: THIS NEEDS TO BE OPTIMIZED FOR DIFFERENT TYPES OF ARRAYS!!!!!!!
  //       Currently this is very inefficient
  intptr_t iLen = unbox_fixnum(len);
  if (iLen == 0)
    return;
  ASSERTF(dest->rank() == 1, BF("dest array must be rank 1 - instead it is %d") % dest->rank());
  ASSERTF(orig->rank() == 1, BF("orig array must be rank 1 - instead it is %d") % orig->rank());
  intptr_t iDestStart = unbox_fixnum(destStart);
  intptr_t iOrigStart = unbox_fixnum(origStart);
  if ((iLen + iDestStart) >= dest->arrayTotalSize()) iLen = dest->arrayTotalSize()-iDestStart;
  if ((iLen + iOrigStart) >= orig->arrayTotalSize()) iLen = orig->arrayTotalSize()-iOrigStart;
  if (iDestStart < iOrigStart) {
    for (cl_index i = 0; i < iLen; ++i) {
      dest->aset_unsafe(iDestStart, orig->aref_unsafe(iOrigStart));
      ++iDestStart;
      ++iOrigStart;
    }
  } else {
    iDestStart += iLen;
    iOrigStart += iLen;
    for (cl_index i = 0; i < iLen; ++i) {
      --iDestStart;
      --iOrigStart;
      dest->aset_unsafe(iDestStart, orig->aref_unsafe(iOrigStart));
    }
  }
}


List_sp listOfObjects(VaList_sp vargs) {
  core::List_sp list = _Nil<core::T_O>();
  va_list cargs;
  va_copy(cargs, (*vargs)._Args);
  size_t nargs = vargs->remaining_nargs();//LCC_VA_LIST_NUMBER_OF_ARGUMENTS(vargs);
  core::Cons_sp *cur = reinterpret_cast<core::Cons_sp *>(&list);
  for (int p = 0; p < nargs; ++p) {
    core::T_sp obj = T_sp((gc::Tagged)va_arg(cargs, T_O *));
    *cur = core::Cons_O::create(obj, _Nil<core::T_O>());
    cur = reinterpret_cast<core::Cons_sp *>(&(*cur)->_Cdr);
  }
  //  va_end(cargs);
  return list;
}



CL_LAMBDA(array &rest indices-value);
CL_DECLARE();
CL_DOCSTRING("aset");
CL_DEFUN T_sp core__aset(Array_sp array, VaList_sp vargs) {
  T_sp last_val;
  cl_index rowMajorIndex = this->index_val_(vargs,true,last_val);
  array->rowMajorAset(rowMajorIndex,last_val);
  return last_val;
};

CL_LISPIFY_NAME("cl:aref");
CL_LAMBDA((core::self cl:array) &va-rest core::indices);
CL_DEFMETHOD T_sp Array_O::aref(VaList_sp indices) const {
  cl_index rowMajorIndex = arrayRowMajorIndex(vargs);
  return array->rowMajorAref(rowMajorIndex,last_val);
}


Symbol_sp Array_O::element_type_as_symbol() const {
  // If this fails we need a different way of doing this
  if ( cl__symbolp(this->elementType()) ) {
    return this->elementType();
  }
  if (this->elementType() == _lisp->_true()) {
    return cl::_sym_T;
  }
  if (this->elementType() == cl__find_class(cl::_sym_DoubleFloat_O) ) {
    return cl::_sym_DoubleFloat_O;
  }
  SIMPLE_ERROR(BF("Handle more array types - the current array type is: %s") % _rep_(this->elementType()));
}


cl_index Array_O::index_vector_int(const vector<int> &indices) const {
  cl_index offset = 0;
  cl_index oneIndex = 0;
  Cons_sp cur;
  cl_index idx = 0;
  for (idx = 0; idx < this->rank(); ++idx) {
    if (idx > 0)
      offset *= this->arrayDimension(idx);
    oneIndex = indices[idx];
    offset += oneIndex;
  }
  return ((offset));
}

cl_index Array_O::index_val_(List_sp indices, bool last_value_is_val, T_sp &last_val) const {
  int indices_passed = cl__length(indices) - (last_value_is_val ? 1 : 0);
#ifdef DEBUG_ON
  ASSERTF(indices_passed == (int)this->rank(),
          BF("Wrong number of indices[%d] must match rank[%d]") % indices_passed % this->rank());
#endif
  cl_index offset = 0;
  cl_index idx = 0;
  cl_index idxEnd(indices_passed);
  List_sp cur = indices;;
  for ( ; idx<idxEnd; ++idx ) {
    T_sp index = oCar(cur);
    cl_index curDimension = this->arrayDimension(idx);
    cl_index oneIndex = clasp_to_int(gc::As<Rational_sp>(index));
    if (oneIndex < 0 || oneIndex >= curDimension) {
      SIMPLE_ERROR(BF("Bad index %d - must be [0,%d)") % curDimension);
    }
    offset = offset * curDimension + oneIndex;
  }
  if (last_value_is_val) last_val = oCar(cur);
  return offset;
}

cl_index Array_O::index_val_(VaList_sp indices, bool last_value_is_val, T_sp &last_val) const {
  int indices_passed = indices->remaining_nargs() - (last_value_is_val ? 1 : 0);
#ifdef DEBUG_ON
  ASSERTF(indices_passed == (int)this->rank(),
          BF("Wrong number of indices[%d] must match rank[%d]") % indices_passed % this->rank());
#endif
  cl_index offset = 0;
  cl_index idx = 0;
  cl_index idxEnd(indices_passed);
  for ( ; idx < idxEnd; ++idx) {
    core::T_sp cur = indices->next_arg();
    cl_index curDimension = this->arrayDimension(idx);
    cl_index oneIndex = clasp_to_int(gc::As<Rational_sp>(cur));
    if (oneIndex < 0 || oneIndex >= curDimension) {
      SIMPLE_ERROR(BF("Bad index %d - must be [0,%d)") % curDimension);
    }
    offset = offset * curDimension + oneIndex;
  }
  if (last_value_is_val) {
    last_val = indices->next_arg();
  }
  return offset;
}

CL_LAMBDA(array &va-rest indices);
CL_LISPIFY_NAME("core:index");
CL_DEFUN gc::Fixnum core__index(Array_sp array, VaList_sp indices) {
  T_sp dummy;
  return array->index_val_(indices, false, dummy);
}

CL_LAMBDA((core::self array) &va-rest core::indices);
CL_LISPIFY_NAME("cl:arrayRowMajorIndex");
CL_DEFMETHOD cl_index Array_O::arrayRowMajorIndex(VaList_sp indices) const {
  return this->index_(indices);
}

CL_LISPIFY_NAME("cl:array-dimensions");
CL_DEFMETHOD List_sp Array_O::arrayDimensions() const {
  _OF();
  List_sp indices = _Nil<T_O>();
  for (int i = this->rank() - 1; i >= 0; i--) {
    indices = Cons_O::create(make_fixnum(this->arrayDimension(i)), indices);
  }
  return ((indices));
}

CL_LAMBDA((core::self array) &rest core::indices-val);
CL_DOCSTRING("Setter for aref");
CL_LISPIFY_NAME("core:array-setf-aref");
CL_DEFMETHOD T_sp Array_O::setf_aref(List_sp indices_val) {
  SUBCLASS_MUST_IMPLEMENT();
};

struct RecursivePrint {
  Array_sp me;
  int depth;
  vector<int> indices;
  stringstream ss;

  RecursivePrint(const Array_sp &array) {
    this->me = array;
    this->indices.resize(array->rank(), 0);
    this->depth = array->rank() - 1;
  }
  void recurse(int level) {
    while (1) {
      if (level < depth) {
        ss << "(";
        recurse(level + 1);
        ss << ")";
      } else {
        ss << _rep_(this->me->rowMajorAref(this->me->index_vector_int(this->indices))) << " ";
      }
      if (!this->advanceIndices(level))
        break;
    }
  }

  bool advanceIndices(int level) {
    this->indices[level]++;
    if (this->indices[level] < this->me->arrayDimension(level))
      return ((true));
    this->indices[level] = 0;
    return ((false));
  }
};

string Array_O::__repr__() const {
  RecursivePrint rp(this->asSmartPtr());
  rp.ss << "#" << this->rank() << "A(";
  rp.recurse(0);
  rp.ss << ")";
  return ((rp.ss.str()));
}

SYMBOL_SC_(CorePkg, copy_subarray);
SYMBOL_SC_(CorePkg, aset);

}; /* core */



// ----------------------------------------------------------------------
//
//  String functions
//


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

static String_sp string_trim0(bool left_trim, bool right_trim, T_sp char_bag, T_sp tstrng) {
  cl_index i, j;
  String_sp strng = coerce::stringDesignator(tstrng);
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
  return strng->subseq(i, clasp_make_fixnum(j-i));
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
CL_DEFUN String_sp cl__string_upcase(T_sp arg) {
  String_sp str = coerce::stringDesignator(arg);
  String_sp result = String_O::create(str);
  for ( cl_index i(0), iEnd(result->length()); i<iEnd; ++i ) {
    result->setf_elt(i,gc::UnsafeAs<Character_sp>(result->elt(i)).char_upcase());
  }
  return (result);
};


CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("string_downcase");
CL_DEFUN String_sp cl__string_downcase(T_sp arg) {
  String_sp str = coerce::stringDesignator(arg);
  String_sp result = String_O::create(str);
  for ( cl_index i(0), iEnd(result->length()); i<iEnd; ++i ) {
    result->setf_elt(i,gc::UnsafeAs<Character_sp>(str->elt(i)).char_downcase());
  }
  return (result);
};


CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("nstring_upcase");
CL_DEFUN String_sp cl__nstring_upcase(String_sp arg) {
  for ( cl_index i(0), iEnd(arg->length()); i<iEnd; ++i ) {
    arg->setf_elt(i,gc::UnsafeAs<Character_sp>(arg->elt(i)).char_upcase());
  }
  return arg;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("nstring_downcase");
CL_DEFUN String_sp cl__nstring_downcase(String_sp arg) {
  for ( cl_index i(0), iEnd(arg->length()); i<iEnd; ++i ) {
    arg->setf_elt(i,gc::UnsafeAs<Character_sp>(arg->elt(i)).char_downcase());
  }
  return arg;
};


CL_LAMBDA(str idx);
CL_DECLARE();
CL_DOCSTRING("char");
CL_DEFUN claspChar cl__char(String_sp ostr, cl_index idx) {
/* Return the character at idx - ignore fill pointers */
  return ostr->elt(idx);
};


bool clasp_memberChar(claspChar c, String_sp charBag) {
  Character_sp cc = clasp_make_character(c);
  for ( cl_index i(0), iEnd(charBag->length()); i<iEnd; ++i ) {
    if (charBag->elt(i) == cc) return true;
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




#if defined(XML_ARCHIVE)
void String_O::archiveBase(::core::ArchiveP node) {
  this->Base::archiveBase(node);
  // Archive other instance variables here
}
#endif // defined(XML_ARCHIVE)

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

String_sp make_string(T_sp element_type,
                      size_t dimension,
                      bool adjustable,
                      T_sp fill_pointer,
                      T_sp displaced_to,
                      cl_index displaced_index_offset,
                      T_sp initial_element,
                      T_sp initial_element_supplied_p)
{
  if (displaced_to.notnilp()) {
    LOG(BF("Creating string displaced to: %s") % _rep_(displaced_to));
    return Str_O::create(dimension, gc::As<Str_sp>(displaced_to), displaced_index_offset);
  } else {
    char c = ' ';
    if (initial_element_supplied_p) {
      c = clasp_as_character(initial_element);
    }
    if (fill_pointer.fixnump()) {
      return Str_O::create_with_fill_pointer(c,dimension,fill_pointer.unsafe_fixnum(),true);
    } else {
      return Str_O::create(c,dimension);
    }
  }
}

};





namespace core {
void VectorNs_O::ensureSpaceAfterFillPointer(cl_index size) {
  if (!this->_FillPointerP) noFillPointerError();
  cl_index left = this->arrayTotalSize() - this->fillPointer();
  if (left < size) {
    this->setSize((size-left)+this->_ArrayTotalSize);
  }
}

T_sp VectorNs_O::vectorPush(T_sp newElement) {
  unlikely_if (!this->_FillPointerp) noFillPointerError();
  cl_index idx = this->_FillPointerOrLengthOrDummy;
  likely_if (idx < this->_ArrayTotalSize) {
    this->_Vec->setf_elt(idx+this->_DisplacedIndexOffset,newElement);
    ++this->_FillPointerOrLengthOrDummy;
    return clasp_make_fixnum(idx);
  }
  return _Nil<T_O>();
}

Fixnum_sp VectorNs_O::vectorPushExtend(T_sp newElement, cl_index extension) {
  unlikely_if (!this->_FillPointerP) noFillPointerError();
  cl_index idx = this->_FillPointerOrLengthOrDummy;
  unlikely_if (idx >= this->_ArrayTotalSize) {
    if (extension <= 0) extension = 32;
  }
  cl_index new_size = this->_ArrayTotalSize+extension;
  unlikely_if (!cl::_sym_adjust_array->boundP()) {
    this->setSize(new_size);
  } else {
    eval::funcall(cl::_sym_adjust_array,this->asSmartPtr(),clasp_make_fixnum(new_size),cl::_sym_fill_pointer,this->_FillPointer);
  }
  this->_Vec->setf_elt(idx+this->_DisplacedIndexOffset,newElement);
  ++this->_FillPointerOrLengthOrDummy;
  return make_fixnum(idx);
}


size_t StrFind(Str_sp outer, Str_sp inner, size_t start)
{
  DEPRECIATED(); // I think only called by Lisp_O::findLoadTimeValuesWithNameContaining
  return clasp_search_string(substr,clasp_make_fixnum(0),_Nil<T_O>(),outer,clasp_make_fixnum(start),_Nil<T_O>());
}
  
};

