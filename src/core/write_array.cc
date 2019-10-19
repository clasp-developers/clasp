/*
    write_array.d -- File interface.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/corePackage.fwd.h>
#include <clasp/core/print.h>
#include <clasp/core/array.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/numberToString.h>

/* NOTE about CL array printing semantics.
 * *print-readably* overrides *print-array*, *print-length*, and *print-level*,
 * in that if *print-readably* is true, these are treated as true, false, and
 * false respectively.
 * Otherwise, if *print-array* is false we don't print array data so the other two
 * are irrelevant.
 *
 * NOTE about our choices.
 * If *print-readably* is true we use a format not in the standard in order to
 * preserve element-type information. We do this even if the element type is T
 * (in which case it's possible to print readably with standard formats).
 * We do not preserve complex array information (e.g. displacement, fill pointers).
 */

namespace core {
// Write the data part of an array representation, e.g. (1 2 3) for a 3-long vector
static void write_array_data(size_t rank, std::vector<size_t> adims, Array_sp array,
                             T_sp stream, size_t print_length, bool write_elems) {
  Fixnum i, j, k, m;
  cl_index subscripts[rank];

  // Special case zero rank arrays.
  if (rank == 0) {
    // ignore write_elems - I think that's correct.
    write_object(array->rowMajorAref(0), stream);
    return;
  }
  
  for (i = 0; i < rank; ++i) subscripts[i] = 0;
  
  for (m = 0, j = 0;;) {
    for (i = j; i < rank; ++i) {
      if (subscripts[i] == 0) {
        clasp_write_char('(', stream);
        if (adims[i] == 0) {
          clasp_write_char(')', stream);
          j = i - 1;
          k = 0;
          goto INC;
        }
      }
      if (subscripts[i] > 0)
        clasp_write_char(' ', stream);
      if (subscripts[i] >= print_length) {
        writestr_stream("...)", stream);
        k = adims[i] - subscripts[i];
        subscripts[i] = 0;
        for (j = i + 1; j < rank; j++)
          k *= adims[j];
        j = i - 1;
        goto INC;
      }
    }
    /* FIXME: This conses, according to an old note. */
    if (write_elems)
      write_object(array->rowMajorAref(m), stream);
    else
      clasp_write_char('#', stream);
    j = rank - 1;
    k = 1;

  INC:
    while (j >= 0) {
      if (++subscripts[j] < adims[j])
        break;
      subscripts[j] = 0;
      clasp_write_char(')', stream);
      --j;
    }
    if (j < 0)
      break;
    m += k;
  }
}

// Write a vector out as a list. Might save consing up an actual Lisp list.
static void write_array_dimensions(size_t rank, std::vector<size_t> dims,
                                   T_sp stream) {
  clasp_write_char('(', stream);
  for (size_t i = 0; i < rank; ++i) {
    write_object(clasp_make_fixnum(dims[i]), stream);
    if (i < rank - 1) clasp_write_char(' ', stream);
  }
  clasp_write_char(')', stream);
}

// Write an array in the extension format for readability,
// #A(ELEMENT-TYPE DIMENSIONS DATA).
// Ignores *print-length*, *print-level*, and of course *print-array*.
static void write_array_ext_readable (Array_sp array, T_sp stream) {
  size_t rank = array->rank();
  std::vector<size_t> adims = array->arrayDimensionsAsVector();
  
  writestr_stream("#A(", stream);
  write_object(array->element_type(), stream); // write element type
  clasp_write_char(' ', stream);
  write_array_dimensions(rank, adims, stream);
  clasp_write_char(' ', stream);
  write_array_data(rank, adims, array, stream, MOST_POSITIVE_FIXNUM, true);
  clasp_write_char(')', stream);
}

// Write an array in the normal #(...data...) or #nA(...data...) format.
// This is not readable unless the element-type is T.
// Ignores *print-array* but respects *print-length*, *print-level*,
// and *print-readably* except for the element type thing.
static void write_array_basic(Array_sp array, T_sp stream) {
  size_t rank = array->rank();
  bool readably = clasp_print_readably();
  Fixnum print_length, print_level;

  if (readably) { // ignore length and level
    print_length = MOST_POSITIVE_FIXNUM;
    print_level = MOST_POSITIVE_FIXNUM;
  } else {
    print_length = clasp_print_length();
    print_level = clasp_print_level();
  }

  clasp_write_char('#', stream);
  if (print_level == 0) return;

  if (rank != 1) { // Need to use #nA
    _clasp_write_fixnum(rank, stream);
    clasp_write_char('A', stream);
  }

  if (print_level >= rank) { // We're writing all elements of the array.
    DynamicScopeManager scope;
    scope.pushSpecialVariableAndSet(cl::_sym_STARprint_levelSTAR, clasp_make_fixnum(print_level - rank));
    write_array_data(rank, array->arrayDimensionsAsVector(), array, stream, print_length, true);
  }
  else // nope.
    // We don't need to set print_level since write_array_data itself ignores it,
    // only the inner write_objects use it, and we won't be doing that.
    write_array_data(print_level, array->arrayDimensionsAsVector(), array, stream, print_length, false);
}

// Write an array as #<[SIMPLE-]ARRAY ELEMENT-TYPE DIMENSIONS> for *print-array* nil.
// FIXME: Maybe we should include the address?
static void write_array_unreadable(Array_sp array, T_sp stream) {
  writestr_stream("#<", stream);
  write_object(array->array_type(), stream); // simple-array or array
  clasp_write_char(' ', stream);
  write_object(array->element_type(), stream);
  clasp_write_char(' ', stream);
  write_array_dimensions(array->rank(), array->arrayDimensionsAsVector(), stream);
  clasp_write_char('>', stream);
}

// Basic method - some overrides are below (FIXME: move them?)
void Array_O::__write__(T_sp stream) const {
  if (clasp_print_readably())
    write_array_ext_readable(this->asSmartPtr(), stream);
  else if (clasp_print_array())
    write_array_basic(this->asSmartPtr(), stream);
  else write_array_unreadable(this->asSmartPtr(), stream);
}

void ComplexVector_O::__write__(T_sp stream) const {
  if (clasp_print_readably() || clasp_print_array()) {
    writestr_stream("#(", stream);
    cl_index length = this->length();
    for (cl_index ndx=0; ndx < length; ++ndx) {
      write_object(this->rowMajorAref(ndx), stream);
      if (ndx < length -1) clasp_write_char(' ', stream);
    }
    clasp_write_char(')', stream);
  } else write_array_unreadable(this->asSmartPtr(), stream);
}

void SimpleBitVector_O::__write__(T_sp stream) const {
  if (clasp_print_readably() || clasp_print_array()) {
    writestr_stream("#*", stream);
    for (cl_index ndx=0; ndx<this->length(); ++ndx)
      if (this->testBit(ndx))
        clasp_write_char('1', stream);
      else
        clasp_write_char('0', stream);
  } else write_array_unreadable(this->asSmartPtr(), stream);
}

// FIXME: Duplicates the above
void BitVectorNs_O::__write__(T_sp stream) const {
  if (clasp_print_readably() || clasp_print_array()) {
    writestr_stream("#*", stream);
    for (cl_index ndx=0; ndx<this->length(); ++ndx)
      if (this->testBit(ndx))
        clasp_write_char('1', stream);
      else
        clasp_write_char('0', stream);
  } else write_array_unreadable(this->asSmartPtr(), stream);
}

void unsafe_write_SimpleBaseString(SimpleBaseString_sp str, size_t start, size_t end, T_sp stream) {
  cl_index ndx;
  if (!clasp_print_escape() && !clasp_print_readably()) {
    for (ndx = start; ndx < end; ndx++) {
      clasp_write_char((*str)[ndx], stream);
    }
  } else {
    clasp_write_char('"', stream);
    for (ndx = start; ndx < end; ndx++) {
      char c = (*str)[ndx];
      if (c == '"' || c == '\\')
        clasp_write_char('\\', stream);
      clasp_write_char(c, stream);
    }
    clasp_write_char('"', stream);
  }
}

void unsafe_write_SimpleCharacterString(SimpleCharacterString_sp str, size_t start, size_t end, T_sp stream) {
  cl_index ndx;
  if (!clasp_print_escape() && !clasp_print_readably()) {
    for (ndx = start; ndx < end; ndx++) {
      clasp_write_char((*str)[ndx],stream);
    }
  } else {
    clasp_write_char('"', stream);
    for (ndx = start; ndx < end; ndx++) {
      claspCharacter c = (*str)[ndx];
      if (c == '"' || c == '\\')
        clasp_write_char('\\', stream);
      clasp_write_char((*str)[ndx],stream);
    }
    clasp_write_char('"', stream);
  }
}
    
void SimpleBaseString_O::__write__(T_sp stream) const {
  unsafe_write_SimpleBaseString(this->asSmartPtr(),0,this->length(),stream);
}

void Str8Ns_O::__write__(T_sp stream) const {
  size_t start, end;
  AbstractSimpleVector_sp str;
  this->asAbstractSimpleVectorRange(str,start,end);
  SimpleBaseString_sp sb = gc::As<SimpleBaseString_sp>(str);
  unsafe_write_SimpleBaseString(sb,start,end,stream);
}
    
void SimpleCharacterString_O::__write__(T_sp stream) const {
  unsafe_write_SimpleCharacterString(this->asSmartPtr(),0,this->length(),stream);
}
    
void StrWNs_O::__write__(T_sp stream) const {
  size_t start, end;
  AbstractSimpleVector_sp str;
  this->asAbstractSimpleVectorRange(str,start,end);
  SimpleCharacterString_sp sc = gc::As<SimpleCharacterString_sp>(str);
  unsafe_write_SimpleCharacterString(sc,start,end,stream);
}

}; // namespace core
