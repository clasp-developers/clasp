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
 * BitUnitBitWidth is the number of bits in an element, e.g. 4 for (signed-byte 4).
 * Operations deal with signed or unsigned chars.
 */
template <size_t BitUnitBitWidth>
class GCBitUnitArray_moveable : public GCContainer {
 public:
#if BIT_ARRAY_BYTE_SIZE==8
  typedef byte8_t word_type;
#elif BIT_ARRAY_BYTE_SIZE==32
  typedef byte32_t word_type;
#elif
#error Unsupported bit array byte size
#endif
  static const size_t bits_in_word = sizeof(word_type)*CHAR_BIT;
  static const size_t bit_unit_bit_width = BitUnitBitWidth;
  static const size_t number_of_bit_units_in_word = bits_in_word/bit_unit_bit_width;
  // Used to deal with endianness.
  static const size_t shift_to_0 = number_of_bit_units_in_word-1;
  static const word_type bit_unit_mask = (((0x1<<bit_unit_bit_width)-1));
  /* Length is measured in units, not words.
   * _Data will have more than _Length elements, because there is more than one unit per word. */
  size_t    _Length;
  word_type _Data[0];
  public:
  GCBitUnitArray_moveable(size_t length, word_type initialValue,
                          bool initialValueSupplied,
                          size_t initialContentsSize = 0, word_type* initialContents=NULL)
  : _Length(length) {
    word_type initialFillValue = (initialValue!=0) ? ~0 : 0;
    // Initialize the contents from an array - but it has to be word_type aligned
    // if you need other than word aligned add another parameter to this constructor
    size_t numWords = sizeof_for_length(length)/sizeof(word_type);
#ifdef DEBUG_BITUNIT_CONTAINER
    printf("%s:%d ctor for GCBitUnitArray_moveable _Data[0] @%p\n", __FILE__, __LINE__, (void*)&this->_Data[0]);
    printf("%s:%d      initialContentsSize = %lu\n", __FILE__, __LINE__, initialContentsSize);
    printf("%s:%d                 numWords = %lu\n", __FILE__, __LINE__, numWords);
#endif
    size_t idx;
    idx = 0;
    for ( ; idx<initialContentsSize; ++idx) this->_Data[idx] = initialContents[idx];
    for ( ; idx<numWords; ++idx ) this->_Data[idx] = initialFillValue;
#ifdef DEBUG_BITUNIT_CONTAINER
    printf("%s:%d done initialization of data in ctor for GCBitUnitArray_moveable _Data[0] @%p\n", __FILE__, __LINE__, (void*)&this->_Data[0]);
    printf("%s:%d      final value of idx = %lu\n", __FILE__, __LINE__, idx);
    printf("%s:%d      wrote up to address: %p\n", __FILE__, __LINE__, (void*)&this->_Data[idx]);
#endif
  }
  // sizeof _Data if _Length is the provided value.
  static size_t sizeof_for_length(size_t length) {
    size_t numWords = (length+(number_of_bit_units_in_word-1))/number_of_bit_units_in_word;
    size_t numBytes = numWords*sizeof(word_type);
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
  size_t number_of_words() const { return sizeof_for_length(this->_Length)/sizeof(word_type);};
  word_type &operator[](size_t i) { return this->_Data[i]; };
  const word_type &operator[](size_t i) const { return this->_Data[i]; };
  /* Unsigned BitUnit access */
  void unsignedSetBitUnit(size_t idx, unsigned char value) {
    // Which word are we hitting?
    size_t block = idx / number_of_bit_units_in_word;
    // Index of the least significant bit of the unit in the word.
    size_t offset = (idx % number_of_bit_units_in_word)*bit_unit_bit_width;
    // Mask for all of the word except the target unit.
    word_type mask = ~(bit_unit_mask <<(shift_to_0-offset));
    // The provided unit shifted into position.
    word_type packedVal = value << (shift_to_0-offset);
    this->_Data[block] = (this->_Data[block] & mask) | packedVal;
  }
  unsigned char unsignedBitUnit(size_t idx) const {
    size_t block = idx / number_of_bit_units_in_word;
    size_t offset = (idx % number_of_bit_units_in_word)*bit_unit_bit_width;
    word_type mask = (bit_unit_mask << (shift_to_0-offset)); 
    unsigned char result = (this->_Data[block] & mask)>>(shift_to_0-offset);
    return result;
  }
  void signedSetBitUnit(size_t idx, char value) {
    size_t block = idx / number_of_bit_units_in_word;
    size_t offset = (idx % number_of_bit_units_in_word)*bit_unit_bit_width;
    word_type mask = ~(bit_unit_mask << (shift_to_0-offset));
    word_type packedVal = (value & bit_unit_mask) << (shift_to_0-offset);
    this->_Data[block] = (this->_Data[block] & mask) | packedVal;
  }
  char signedBitUnit(size_t idx) const {
    size_t block = idx / number_of_bit_units_in_word;
    size_t offset = (idx % number_of_bit_units_in_word)*bit_unit_bit_width;
    size_t right_shift = bits_in_word-(shift_to_0-offset)-bit_unit_bit_width;
    word_type mask = (bit_unit_mask << (shift_to_0-offset));
    char result = (this->_Data[block] & mask)<<right_shift; // logical shift right
    result = result >> (bits_in_word-bit_unit_bit_width);
    return result;
  }
};
} // namespace gctools

#endif
