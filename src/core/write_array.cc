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

#define DEBUG_DENSE 0

#include <bitset>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/corePackage.fwd.h>
#include <clasp/core/print.h>
#include <clasp/core/array.h>
#include <clasp/core/array_float.h>
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
static void write_array_data(size_t rank, std::vector<size_t> adims, Array_sp array, T_sp stream, size_t print_length,
                             bool write_elems) {
  Fixnum i, j, k, m;
  cl_index subscripts[rank];

  // Special case zero rank arrays.
  if (rank == 0) {
    // ignore write_elems - I think that's correct.
    write_object(array->rowMajorAref(0), stream);
    return;
  }

  for (i = 0; i < rank; ++i)
    subscripts[i] = 0;

  for (m = 0, j = 0;;) {
    for (i = j; i < rank; ++i) {
      if (subscripts[i] == 0) {
        stream_write_char(stream, '(');
        if (adims[i] == 0) {
          stream_write_char(stream, ')');
          j = i - 1;
          k = 0;
          goto INC;
        }
      }
      if (subscripts[i] > 0)
        stream_write_char(stream, ' ');
      if (subscripts[i] >= print_length) {
        clasp_write_string("...)", stream);
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
      stream_write_char(stream, '#');
    j = rank - 1;
    k = 1;

  INC:
    while (j >= 0) {
      if (++subscripts[j] < adims[j])
        break;
      subscripts[j] = 0;
      stream_write_char(stream, ')');
      --j;
    }
    if (j < 0)
      break;
    m += k;
  }
}

// Write a vector out as a list. Might save consing up an actual Lisp list.
static void write_array_dimensions(size_t rank, std::vector<size_t> dims, T_sp stream) {
  stream_write_char(stream, '(');
  for (size_t i = 0; i < rank; ++i) {
    write_object(clasp_make_fixnum(dims[i]), stream);
    if (i < rank - 1)
      stream_write_char(stream, ' ');
  }
  stream_write_char(stream, ')');
}

// Generae dense (6-bit) character strings from blobs of bytes
// See the reverse function denseReadTo8Bit
std::vector<char> denseWriteTo6Bit(const unsigned char* inputBytes, size_t byteCount) {
  std::vector<char> result;

#define CODING
#include "dense_specialized_array_dispatch.cc"
#undef CODING

  // Initialize variables to keep track of remaining bits from the previous byte
  size_t total8bits = 0;
  size_t total6bits = 0;
  unsigned int remainingBits = 0;
  unsigned int previousBits = 0;
#if DEBUG_DENSE
  std::stringstream sout8;
  std::stringstream sout6;
#endif

  // Iterate through each byte in the input vector
  for (size_t i = 0; i < byteCount; ++i) {
    // Combine the remaining bits from the previous byte with the current byte
#if DEBUG_DENSE
    std::bitset<8> bits(inputBytes[i]);
    sout8 << bits;
#endif
    unsigned int currentByte = (previousBits << 8) | inputBytes[i];
    total8bits += 8;

    // Update the number of remaining bits
    remainingBits += 8;

    // Continue until there are at least 6 bits to extract
    while (remainingBits >= 6) {
      // Extract the next 6 bits - the most significant bits
      unsigned char sixBitValue = (currentByte >> (remainingBits - 6)) & 0x3F;
#if DEBUG_DENSE
      std::bitset<6> bits6(sixBitValue);
      sout6 << bits6;
#endif
      // Map the 6-bit value to a printable character (assuming printable ASCII characters)
      char printableChar = coding[sixBitValue];

      // Add the printable character to the result vector
      result.push_back(printableChar);
      total6bits += 6;

      // Update variables for the next iteration
      remainingBits -= 6;
    }

    // Save the remaining bits for the next iteration
    previousBits = currentByte & ((1 << remainingBits) - 1);
  }

  // Handle any remaining bits after processing all bytes
  if (remainingBits > 0) {
    total6bits += remainingBits;
    unsigned char sixBitValue = (previousBits << (6 - remainingBits));
#if DEBUG_DENSE
    std::bitset<6> bits6(sixBitValue);
    // std::cout << __FILE__ << ":" << __LINE__ << ":" << __FUNCTION__ << " previousBits= " << bits6 << "\n";
    sout6 << bits6;
#endif
    char printableChar = coding[sixBitValue];
    result.push_back(printableChar);
  }

  if (total6bits != total8bits) {
    SIMPLE_ERROR("total8bits {} must match total6bits {}", total8bits, total6bits);
  }
#if DEBUG_DENSE
  printf("%s:%d:%s bit8 stream\n%s\n", __FILE__, __LINE__, __FUNCTION__, sout8.str().c_str());
  printf("%s\n%s:%d:%s bit6 stream\n", sout6.str().c_str(), __FILE__, __LINE__, __FUNCTION__);
#endif
  return result;
}

void write_array_readable_dense(T_sp stream, const std::string kind, void* start, void* end) {
  size_t size = ((const char*)end) - ((const char*)start);
  auto result = denseWriteTo6Bit((const unsigned char*)start, size);
  clasp_write_string("#", stream);
  std::string num = std::to_string(result.size());
  clasp_write_string(num.c_str(), stream);
  clasp_write_string("D", stream);
  clasp_write_string(kind.c_str(), stream);
  clasp_write_characters(&result[0], result.size(), stream);
  clasp_write_string(" ", stream);
}

#define DISPATCH(_vtype_, _type_, _code_)                                                                                          \
  if (gc::IsA<SimpleVector_##_vtype_##_sp>(array)) {                                                                               \
    auto athis = gc::As_unsafe<SimpleVector_##_vtype_##_sp>(array);                                                                \
    write_array_readable_dense(stream, _code_, athis->rowMajorAddressOfElement_(0),                                                \
                               athis->rowMajorAddressOfElement_(cl__length(athis)));                                               \
    return true;                                                                                                                   \
  } else if (gc::IsA<MDArray_##_vtype_##_sp>(array)) {                                                                             \
    auto athis = gc::As_unsafe<MDArray_##_vtype_##_sp>(array);                                                                     \
    write_array_readable_dense(stream, _code_, athis->rowMajorAddressOfElement_(0),                                                \
                               athis->rowMajorAddressOfElement_(cl__length(athis)));                                               \
    return true;                                                                                                                   \
  } else if (gc::IsA<SimpleMDArray_##_vtype_##_sp>(array)) {                                                                       \
    auto athis = gc::As_unsafe<SimpleMDArray_##_vtype_##_sp>(array);                                                               \
    write_array_readable_dense(stream, _code_, athis->rowMajorAddressOfElement_(0),                                                \
                               athis->rowMajorAddressOfElement_(cl__length(athis)));                                               \
    return true;                                                                                                                   \
  } else if (gc::IsA<ComplexVector_##_vtype_##_sp>(array)) {                                                                       \
    auto athis = gc::As_unsafe<ComplexVector_##_vtype_##_sp>(array);                                                               \
    write_array_readable_dense(stream, _code_, athis->rowMajorAddressOfElement_(0),                                                \
                               athis->rowMajorAddressOfElement_(cl__length(athis)));                                               \
    return true;                                                                                                                   \
  }
CL_DEFUN bool core__write_dense_specialized_array(T_sp stream, T_sp array) {

#define DISPATCHES
#include "dense_specialized_array_dispatch.cc"
#undef DISPATCHES

  return false;
}

// Write an array in the extension format for readability,
// #A(ELEMENT-TYPE DIMENSIONS DATA).
// Ignores *print-length*, *print-level*, and of course *print-array*.
static void write_array_ext_readable(Array_sp array, std::vector<size_t> adims, T_sp stream) {
  size_t rank = array->rank();
  bool wrote_binary = false;
  if (clasp_print_dense() && rank == 1) {
    wrote_binary = core__write_dense_specialized_array(stream, array);
  }
  if (!wrote_binary) {
    clasp_write_string("#A(", stream);
#if 0
    if (array->element_type()==cl::_sym_single_float) {
      printf("%s:%d:%s readable single-float vector\n", __FILE__, __LINE__, __FUNCTION__ );
    }
#endif
    write_object(array->element_type(), stream); // write element type
    stream_write_char(stream, ' ');
    write_array_dimensions(rank, adims, stream);
    stream_write_char(stream, ' ');
    write_array_data(rank, adims, array, stream, MOST_POSITIVE_FIXNUM, true);
    stream_write_char(stream, ')');
  }
}

// Write an array in the normal #(...data...) or #nA(...data...) format.
// This is not readable unless the element-type is T.
// Ignores *print-array* but respects *print-length*, *print-level*,
// and *print-readably* except for the element type thing.
static void write_array_basic(Array_sp array, std::vector<size_t> adims, T_sp stream) {
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

  stream_write_char(stream, '#');
  if (print_level == 0)
    return;

  if (rank != 1) { // Need to use #nA
    _clasp_write_fixnum(rank, stream);
    stream_write_char(stream, 'A');
  }

  if (print_level >= rank) { // We're writing all elements of the array.
    DynamicScopeManager scope(cl::_sym_STARprint_levelSTAR, clasp_make_fixnum(print_level - rank));
    write_array_data(rank, adims, array, stream, print_length, true);
  } else // nope.
    // We don't need to set print_level since write_array_data itself ignores it,
    // only the inner write_objects use it, and we won't be doing that.
    write_array_data(print_level, adims, array, stream, print_length, false);
}

// Write an array as #<[SIMPLE-]ARRAY ELEMENT-TYPE DIMENSIONS> for *print-array* nil.
// FIXME: Maybe we should include the address?
static void write_array_unreadable(Array_sp array, std::vector<size_t> adims, T_sp stream) {
  clasp_write_string("#<", stream);
  write_object(array->array_type(), stream); // simple-array or array
  stream_write_char(stream, ' ');
  write_object(array->element_type(), stream);
  stream_write_char(stream, ' ');
  write_array_dimensions(array->rank(), adims, stream);
  stream_write_char(stream, '>');
}

// Basic method - some overrides are below (FIXME: move them?)
void Array_O::__write__(T_sp stream) const {
  std::vector<size_t> adims = this->arrayDimensionsAsVector();
  if (clasp_print_readably()) {
    write_array_ext_readable(this->asSmartPtr(), adims, stream);
  } else if (clasp_print_array())
    write_array_basic(this->asSmartPtr(), adims, stream);
  else
    write_array_unreadable(this->asSmartPtr(), adims, stream);
}

void Array_O::__writeString(size_t istart, size_t iend, T_sp stream) const {
  // Linker seems to require that
  SUBIMP();
}

// This separate method is necessary because we need to account for fill pointers -
// the basic stuff above uses array dimensions, so it's not appropriate.
void ComplexVector_O::__write__(T_sp stream) const {
  std::vector<size_t> adims;
  adims.push_back(this->length()); // fill pointer
  if (clasp_print_readably())
    write_array_ext_readable(this->asSmartPtr(), adims, stream);
  else if (clasp_print_array())
    write_array_basic(this->asSmartPtr(), adims, stream);
  // NOTE: We could use the dimension here, I guess? Who uses *print-array* nil anyway...
  else
    write_array_unreadable(this->asSmartPtr(), adims, stream);
}

void SimpleBitVector_O::__write__(T_sp stream) const {
  if (clasp_print_readably() || clasp_print_array()) {
    clasp_write_string("#*", stream);
    for (cl_index ndx = 0; ndx < this->length(); ++ndx)
      if (this->testBit(ndx))
        stream_write_char(stream, '1');
      else
        stream_write_char(stream, '0');
  } else
    write_array_unreadable(this->asSmartPtr(), this->arrayDimensionsAsVector(), stream);
}

// FIXME: Duplicates the above
void BitVectorNs_O::__write__(T_sp stream) const {
  if (clasp_print_readably() || clasp_print_array()) {
    clasp_write_string("#*", stream);
    for (cl_index ndx = 0; ndx < this->length(); ++ndx)
      if (this->testBit(ndx))
        stream_write_char(stream, '1');
      else
        stream_write_char(stream, '0');
  } else
    write_array_unreadable(this->asSmartPtr(), this->arrayDimensionsAsVector(), stream);
}

void unsafe_write_SimpleBaseString(SimpleBaseString_sp str, size_t start, size_t end, T_sp stream) {
  cl_index ndx;
  if (!clasp_print_escape() && !clasp_print_readably()) {
    for (ndx = start; ndx < end; ndx++) {
      claspChar c = str[ndx];
      stream_write_char(stream, c);
    }
  } else {
    stream_write_char(stream, '"');
    for (ndx = start; ndx < end; ndx++) {
      claspChar c = str[ndx];
      if (c == '"' || c == '\\')
        stream_write_char(stream, '\\');
      stream_write_char(stream, c);
    }
    stream_write_char(stream, '"');
  }
}

void unsafe_write_SimpleCharacterString(SimpleCharacterString_sp str, size_t start, size_t end, T_sp stream) {
  cl_index ndx;
  if (!clasp_print_escape() && !clasp_print_readably()) {
    for (ndx = start; ndx < end; ndx++) {
      stream_write_char(stream, str[ndx]);
    }
  } else {
    stream_write_char(stream, '"');
    for (ndx = start; ndx < end; ndx++) {
      claspCharacter c = str[ndx];
      if (c == '"' || c == '\\')
        stream_write_char(stream, '\\');
      stream_write_char(stream, str[ndx]);
    }
    stream_write_char(stream, '"');
  }
}

void SimpleBaseString_O::__write__(T_sp stream) const {
  unsafe_write_SimpleBaseString(this->asSmartPtr(), 0, this->length(), stream);
}

void Str8Ns_O::__write__(T_sp stream) const {
  size_t start, end;
  AbstractSimpleVector_sp str;
  this->asAbstractSimpleVectorRange(str, start, end);
  SimpleBaseString_sp sb = gc::As<SimpleBaseString_sp>(str);
  unsafe_write_SimpleBaseString(sb, start, end, stream);
}

void SimpleCharacterString_O::__write__(T_sp stream) const {
  unsafe_write_SimpleCharacterString(this->asSmartPtr(), 0, this->length(), stream);
}

void StrWNs_O::__write__(T_sp stream) const {
  size_t start, end;
  AbstractSimpleVector_sp str;
  this->asAbstractSimpleVectorRange(str, start, end);
  SimpleCharacterString_sp sc = gc::As<SimpleCharacterString_sp>(str);
  unsafe_write_SimpleCharacterString(sc, start, end, stream);
}

void StrWNs_O::__writeString(size_t istart, size_t iend, T_sp stream) const {
  size_t start, end;
  AbstractSimpleVector_sp str;
  this->asAbstractSimpleVectorRange(str, start, end);
  SimpleCharacterString_sp sc = gc::As<SimpleCharacterString_sp>(str);
  sc->__writeString(start + istart, start + iend, stream);
}

void Str8Ns_O::__writeString(size_t istart, size_t iend, T_sp stream) const {
  size_t start, end;
  AbstractSimpleVector_sp str;
  this->asAbstractSimpleVectorRange(str, start, end);
  SimpleBaseString_sp sb = gc::As<SimpleBaseString_sp>(str);
  sb->__writeString(start + istart, start + iend, stream);
}

void SimpleBaseString_O::__writeString(size_t start, size_t end, T_sp stream) const {
  for (cl_index ndx = start; ndx < end; ndx++) {
    stream_write_char(stream, (*this)[ndx]);
  }
}

void SimpleCharacterString_O::__writeString(size_t start, size_t end, T_sp stream) const {
  for (cl_index ndx = start; ndx < end; ndx++) {
    stream_write_char(stream, (*this)[ndx]);
  }
}

}; // namespace core
