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

#include <type_traits> // for std::conditional

namespace gctools {

/* Underlying type for arrays of sub-byte elements.
 * The array is represented as a C++ array of "words", a larger type chosen for efficiency.
 * (The bit_array_word type, defined in configure_clasp.h.)
 * BitUnitBitWidth is the number of bits per element, e.g. 4 for (signed-byte 4).
 * Operations deal with signed or unsigned chars.
 * Various things assume two's complement.
 */
template <size_t BitUnitBitWidth, bool signedp>
class GCBitUnitArray_moveable : public GCContainer {
 public:
  static const size_t bits_in_word = sizeof(bit_array_word)*CHAR_BIT;
  static const size_t bit_unit_bit_width = BitUnitBitWidth;
  static const size_t number_of_bit_units_in_word = bits_in_word/bit_unit_bit_width;
  // Used to deal with endianness.
  static const size_t shift_to_0 = number_of_bit_units_in_word-1;
  static const bit_array_word bit_unit_mask = (((0x1<<bit_unit_bit_width)-1));
  // Used for sign stuff.
  static const unsigned char sign_mask = 1 << (bit_unit_bit_width - 1);
  static const unsigned char magnitude_mask = sign_mask - 1;
  /* Length is measured in units, not words.
   * _Data will have fewer than _Length elements, because there is more than one unit per word. */
  size_t    _Length;
  bit_array_word _Data[0];
 public:
  // Type for bit units, as used by the external inteface.
  typedef typename std::conditional<signedp, signed char, unsigned char>::type value_type;
 public:
 GCBitUnitArray_moveable(size_t length, bit_array_word initialValue,
                         bool initialValueSupplied,
                         size_t initialContentsSize = 0, const bit_array_word* initialContents=NULL)
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
  // Given an initial element, replicate it into a bit_array_word. E.g. 01 becomes 01010101...01
  // Hopefully the compiler unrolls the loop.
  static bit_array_word initialFillValue(value_type initialValue) {
    bit_array_word result = unsign(initialValue);
    // There's probably a way to express this with template specialization but damn if I can
    // figure it out
    if (BitUnitBitWidth == 1) {
      if (result != 0) result = ~0;
    } else for (size_t i = BitUnitBitWidth; i < BIT_ARRAY_WORD_BITS; i *= 2) result |= result << i;
    return result;
  }
 public:
  /* Word access */
  bit_array_word &operator[](size_t i) { return this->_Data[i]; };
  const bit_array_word &operator[](size_t i) const { return this->_Data[i]; };
 private:
  // For signedness. We do our usual operations on raw bits, but may need to turn them to
  // an unsigned char.
  // Only one unsign is actually used in any given instantiation. Maybe some template magic
  // could make the other go away? But it doesn't seem important.
  static unsigned char unsign(signed char pvalue) {
    // Low n-1 bits, and then the sign bit.
    // Note that the & is kind of sketchy on a signed type - we assume two's complement.
    return (pvalue & magnitude_mask) | ((pvalue < 0) ? sign_mask : 0);
  }
  static unsigned char unsign(unsigned char pvalue) { return pvalue; }
  static value_type sign(unsigned char pvalue) {
    if (signedp) // template constant
      return (pvalue & magnitude_mask) - (pvalue & sign_mask);
    else return pvalue;
  }
  /* Unsigned BitUnit access */
  // This function is currently unused, but since it avoids any overhead from the "reference"
  // class below, it might be useful.
  void setBitUnit(size_t idx, value_type pvalue) {
    // If value_type is signed, convert to unsigned.
    unsigned char value = unsign(pvalue);
    // Which word are we hitting?
    size_t block = idx / number_of_bit_units_in_word;
    // Index of the least significant bit in the word.
    size_t offset = (shift_to_0 - (idx % number_of_bit_units_in_word))*bit_unit_bit_width;
    // Mask for all of the word except the target unit.
    bit_array_word mask = ~(bit_unit_mask << offset);
    // The provided unit shifted into position.
    // NOTE: The & bit_unit_mask shouldn't be necessary if VALUE is always valid,
    // but I get bad results without it.
    bit_array_word packedVal = (value & bit_unit_mask) << offset;
    this->_Data[block] = (this->_Data[block] & mask) | packedVal;
  }
  value_type bitUnit(size_t idx) const {
    size_t block = idx / number_of_bit_units_in_word;
    size_t offset = (shift_to_0 - (idx % number_of_bit_units_in_word))*bit_unit_bit_width;
    bit_array_word mask = bit_unit_mask << offset;
    unsigned char result = (this->_Data[block] & mask) >> offset;
    return sign(result);
  }
  // Get a pointer to the word the given index uses.
  bit_array_word* word(size_t idx) { return &(this->_Data[idx / number_of_bit_units_in_word]); }
  // Get the offset value for the given index.
  size_t offset(size_t idx) {
    return (shift_to_0 - (idx % number_of_bit_units_in_word))*bit_unit_bit_width;
  }
 public:
  class reference {
    friend class GCBitUnitArray_moveable;
    bit_array_word* word;
    size_t offset;
  public:
    // The following is to support operator[] in higher classes.
    // The idea is taken from std::bitset<n>::reference, and more specifically
    // libgcc's version.
    // I don't know why this constructor gets a reference instead of a pointer.
    reference(GCBitUnitArray_moveable& arr, size_t idx) noexcept {
      word = arr.word(idx);
      offset = arr.offset(idx);
    }
    reference(const reference&) = default;
    ~reference() noexcept {}
    // arr[i] = x
    reference& operator=(value_type x) noexcept {
        bit_array_word packedVal = (unsign(x) & bit_unit_mask) << offset;
        bit_array_word mask = ~(bit_unit_mask << offset);
        *word = (*word & mask) | packedVal;
        return *this;
    }
    // Get raw bits
    unsigned char raw() const noexcept {
      bit_array_word mask = bit_unit_mask << offset;
      return (*word & mask) >> offset;
    }
    // x = arr[i]
    operator value_type() const noexcept { return sign(raw()); }
    // a[i] = b[i], and yes this is required.
    // I don't understand C++ deeply enough, luckily, but a[i] = b[i]
    // seems to be a nop without this.
    reference& operator=(const reference& x) noexcept {
        bit_array_word packedVal = (x.raw() & bit_unit_mask) << offset;
        bit_array_word mask = ~(bit_unit_mask) << offset;
        *word = (*word & mask) | packedVal;
        return *this;
    }
  };
  friend class reference;
  // Like operator[] but for a sub byte unit.
  reference ref(size_t idx) { return reference(*this, idx); }
  value_type ref(size_t idx) const { return bitUnit(idx); }
};
} // namespace gctools

#endif
