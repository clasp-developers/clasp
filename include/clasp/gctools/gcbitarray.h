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
#if BIT_ARRAY_BYTE_SIZE==8
  template <int BitUnitBitWidth, class UnsignedWordType=byte8_t /*uintptr_t*/, class SignedWordType=int8_t/*intptr_t*/>
#elif BIT_ARRAY_BYTE_SIZE==32
  template <int BitUnitBitWidth, class UnsignedWordType=byte32_t /*uintptr_t*/, class SignedWordType=int32_t/*intptr_t*/>
#endif
    class GCBitUnitArray_moveable : public GCContainer {
  public:
  typedef UnsignedWordType word_type;
  typedef SignedWordType signed_word_type;
  static const size_t bits_in_word = sizeof(word_type)*CHAR_BIT;
  static const size_t bit_unit_bit_width = BitUnitBitWidth;
  static const size_t number_of_bit_units_in_word = sizeof(word_type)*CHAR_BIT/bit_unit_bit_width;
  static const size_t number_of_bit_units_in_word_times_bit_unit_bit_width = number_of_bit_units_in_word*bit_unit_bit_width;
  static const size_t shift_to_0 = (((sizeof(word_type)*CHAR_BIT)/bit_unit_bit_width)-1);
  static const word_type bit_unit_mask = (((0x1<<bit_unit_bit_width)-1));
  size_t    _Length; // Index one beyond the total number of elements allocated
  word_type _Data[0];      // Store _Length numbers of bits with multiple bits per T
  public:
  GCBitUnitArray_moveable(size_t length, word_type initialValue, bool initialValueSupplied, size_t initialContentsSize = 0, word_type* initialContents=NULL) : _Length(length) {
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
    void unsignedSetBitUnit(size_t idx, word_type v) {
      size_t block = idx / number_of_bit_units_in_word;
      size_t offset = (idx % number_of_bit_units_in_word)*bit_unit_bit_width;
      word_type mask = ~(bit_unit_mask <<(shift_to_0-offset));
      word_type packedVal = v << (shift_to_0-offset);
#if 0
      printf("%s:%d  idx -> %u\n", __FILE__, __LINE__, (uint)idx);
      printf("%s:%d  v -> %u\n", __FILE__, __LINE__, (uint)v);
      printf("%s:%d  shift_to_0 -> %lu\n", __FILE__, __LINE__, shift_to_0);
      printf("%s:%d  mask -> 0x%x\n", __FILE__, __LINE__, mask);
      printf("%s:%d packedVal -> 0x%x\n", __FILE__, __LINE__, packedVal);
#endif
      this->_Data[block] = (this->_Data[block] & mask) | packedVal;
    }
    word_type unsignedBitUnit(size_t idx) const {
      size_t block = idx / number_of_bit_units_in_word;
      size_t offset = (idx % number_of_bit_units_in_word)*bit_unit_bit_width;
      word_type mask = (bit_unit_mask << (shift_to_0-offset));
      GC_LOG(BF("testBit i=%u number_of_bit_units_in_word=%d block=%d offset=%d") % (idx) % number_of_bit_units_in_word % (block) % (offset));
      GC_LOG(BF("      mask = |%lx|") % mask);
      GC_LOG(BF("bitunitss[%04d] = |%lx|") % block % this->_Data[block]);
      word_type result = (this->_Data[block] & mask)>>(shift_to_0-offset);
      GC_LOG(BF("    result = |%lx|") % result);
      return result;
    }
    void signedSetBitUnit(size_t idx, signed_word_type v) {
      size_t block = idx / number_of_bit_units_in_word;
      size_t offset = (idx % number_of_bit_units_in_word)*bit_unit_bit_width;
      word_type mask = ~(bit_unit_mask << (shift_to_0-offset));
      word_type packedVal = (v&bit_unit_mask) << (shift_to_0-offset);
      this->_Data[block] = (this->_Data[block] & mask) | packedVal;
    }
    signed_word_type signedBitUnit(size_t idx) const {
      size_t block = idx / number_of_bit_units_in_word;
      size_t offset = (idx % number_of_bit_units_in_word)*bit_unit_bit_width;
      size_t right_shift = bits_in_word-(shift_to_0-offset)-bit_unit_bit_width;
      word_type mask = (bit_unit_mask << (shift_to_0-offset));
      GC_LOG(BF("testBit i=%u number_of_bit_units_in_word=%d block=%d offset=%d") % (idx) % number_of_bit_units_in_word % (block) % (offset));
      GC_LOG(BF("      mask = |%lx|") % mask);
      GC_LOG(BF("bitunitss[%04d] = |%lx|") % block % this->_Data[block]);
      signed_word_type result = (this->_Data[block] & mask)<<right_shift; // logical shift right
      result = result >> (bits_in_word-bit_unit_bit_width);
      GC_LOG(BF("    result = |%lx|") % result);
      return result;
    }
  };
} // namespace gctools

#endif
