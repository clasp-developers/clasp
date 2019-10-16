/*
    File: gcbitarray.h
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
#ifndef gc_gcbitarray_H
#define gc_gcbitarray_H

namespace gctools {

/* Underlying type for arrays of sub-byte elements.
 * The array is represented as a C++ array of "words", a larger type chosen for efficiency.
 * (The bit_array_word type, defined in configure_clasp.h.)
 * BitUnitBitWidth is the number of bits per element, e.g. 4 for (signed-byte 4).
 * Operations deal with signed or unsigned chars.
 */
template <size_t BitUnitBitWidth>
class GCBitUnitArray_moveable : public GCContainer {
 public:
  static const size_t bits_in_word = sizeof(bit_array_word)*CHAR_BIT;
  static const size_t bit_unit_bit_width = BitUnitBitWidth;
  static const size_t number_of_bit_units_in_word = bits_in_word/bit_unit_bit_width;
  // Used to deal with endianness.
  static const size_t shift_to_0 = number_of_bit_units_in_word-1;
  static const bit_array_word bit_unit_mask = (((0x1<<bit_unit_bit_width)-1));
  /* Length is measured in units, not words.
   * _Data will have fewer than _Length elements, because there is more than one unit per word. */
  size_t    _Length;
  bit_array_word _Data[0];
  public:
 GCBitUnitArray_moveable(size_t length, bit_array_word initialValue,
                         bool initialValueSupplied,
                         size_t initialContentsSize = 0, bit_array_word* initialContents=NULL)
   : _Length(length) {
    // Initialize the contents from an array - but it has to be bit_array_word aligned
    // if you need other than word aligned add another parameter to this constructor
    size_t numWords = nwords_for_length(length);
#ifdef DEBUG_BITUNIT_CONTAINER
    printf("%s:%d ctor for GCBitUnitArray_moveable _Data[0] @%p\n", __FILE__, __LINE__, (void*)&this->_Data[0]);
    printf("%s:%d      initialContentsSize = %lu\n", __FILE__, __LINE__, initialContentsSize);
    printf("%s:%d                 numWords = %lu\n", __FILE__, __LINE__, numWords);
#endif
    size_t idx;
    idx = 0;
    for ( ; idx<initialContentsSize; ++idx) this->_Data[idx] = initialContents[idx];
    for ( ; idx<numWords; ++idx ) this->_Data[idx] = initialValue;
#ifdef DEBUG_BITUNIT_CONTAINER
    printf("%s:%d done initialization of data in ctor for GCBitUnitArray_moveable _Data[0] @%p\n", __FILE__, __LINE__, (void*)&this->_Data[0]);
    printf("%s:%d      final value of idx = %lu\n", __FILE__, __LINE__, idx);
    printf("%s:%d      wrote up to address: %p\n", __FILE__, __LINE__, (void*)&this->_Data[idx]);
#endif
  }
 GCBitUnitArray_moveable(size_t length, bit_array_word* initialContents)
   : _Length(length) {
    for (size_t i = 0; i < nwords_for_length(length); ++i)
      this->_Data[i] = initialContents[i];
  }
  static size_t nwords_for_length(size_t length) {
    // length/number_of_bit_units_in_word, rounded up
    return (length+number_of_bit_units_in_word-1)/number_of_bit_units_in_word;
  }
  // sizeof _Data if _Length is the provided value.
  static size_t sizeof_for_length(size_t length) {
    size_t numWords = (length+number_of_bit_units_in_word-1)/number_of_bit_units_in_word;
    size_t numBytes = numWords*sizeof(bit_array_word);
#ifdef DEBUG_BITUNIT_CONTAINER
    printf("%s:%d length = %lu\n", __FILE__, __LINE__, length);
    printf("%s:%d number_of_bit_units_in_word = %lu\n", __FILE__, __LINE__, number_of_bit_units_in_word);
    printf("%s:%d numWords = %lu\n", __FILE__, __LINE__, numWords );
    printf("%s:%d numBytes = %lu\n", __FILE__, __LINE__, numBytes );
#endif
    return numBytes;
  }
  public:
    /* Word access */
  bit_array_word &operator[](size_t i) { return this->_Data[i]; };
  const bit_array_word &operator[](size_t i) const { return this->_Data[i]; };
  /* Unsigned BitUnit access */
  void unsignedSetBitUnit(size_t idx, unsigned char value) {
    // Which word are we hitting?
    size_t block = idx / number_of_bit_units_in_word;
    // Index of the least significant bit of the unit in the word.
    size_t offset = (idx % number_of_bit_units_in_word)*bit_unit_bit_width;
    // Mask for all of the word except the target unit.
    bit_array_word mask = ~(bit_unit_mask <<(shift_to_0-offset));
    // The provided unit shifted into position.
    // NOTE: The & bit_unit_mask shouldn't be necessary if VALUE is always valid,
    // but I get bad results without it.
    bit_array_word packedVal = (value & bit_unit_mask) << (shift_to_0-offset);
    this->_Data[block] = (this->_Data[block] & mask) | packedVal;
  }
  unsigned char unsignedBitUnit(size_t idx) const {
    size_t block = idx / number_of_bit_units_in_word;
    size_t offset = (idx % number_of_bit_units_in_word)*bit_unit_bit_width;
    bit_array_word mask = (bit_unit_mask << (shift_to_0-offset)); 
    unsigned char result = (this->_Data[block] & mask)>>(shift_to_0-offset);
    return result;
  }
};
} // namespace gctools

#endif
