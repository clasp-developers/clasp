#pragma once

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

#include <type_traits> // for std::conditional
#include <iterator>

namespace gctools {

// Used in the object scanner to determine size.
// Just like sizeof_for_length below, but with variable bitunit size.
// NOTE: The "inline" is necessary to prevent multiple clashing definitions.
inline size_t bitunit_sizeof(size_t bit_unit_bit_width, size_t capacity) {
  size_t number_of_bit_units_in_word = CHAR_BIT * sizeof(bit_array_word) / bit_unit_bit_width;
  // Integer division but rounding up. Little awkward.
  size_t numWords = (capacity + number_of_bit_units_in_word - 1) / number_of_bit_units_in_word;
  return numWords * sizeof(bit_array_word);
}

/* Underlying type for arrays of sub-byte elements.
 * The array is represented as a C++ array of "words", a larger type chosen for efficiency.
 * (The bit_array_word type, defined in configure_clasp.h.)
 * BitUnitBitWidth is the number of bits per element, e.g. 4 for (signed-byte 4).
 * Operations deal with signed or unsigned chars.
 * Various things assume two's complement.
 */
template <int BitUnitBitWidth, int Signedp> class GCBitUnitArray_moveable : public GCContainer {
public:
  GCBitUnitArray_moveable(){};

public:
  static const size_t bits_in_word = sizeof(bit_array_word) * CHAR_BIT;
  static const size_t bit_unit_bit_width = BitUnitBitWidth;
  static const size_t number_of_bit_units_in_word = bits_in_word / bit_unit_bit_width;
  // Used to deal with endianness.
  static const size_t shift_to_0 = number_of_bit_units_in_word - 1;
  static const bit_array_word bit_unit_mask = (((0x1 << bit_unit_bit_width) - 1));
  // Used for sign stuff.
  static const unsigned char sign_mask = 1 << (bit_unit_bit_width - 1);
  static const unsigned char magnitude_mask = sign_mask - 1;
  /* Length is measured in units, not words.
   * _Data will have fewer than _Length elements, because there is more than one unit per word. */
  size_t _Length;
  bit_array_word _Data[0];

public:
  // Type for bit units, as used by the external interface.
  typedef typename std::conditional<Signedp != 0, signed char, unsigned char>::type value_type;

public:
  GCBitUnitArray_moveable(size_t length, bit_array_word initialValue, bool initialValueSupplied, size_t initialContentsSize = 0,
                          const bit_array_word* initialContents = NULL)
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
    for (; idx < initialContentsSize; ++idx)
      this->_Data[idx] = initialContents[idx];
    for (; idx < numWords; ++idx)
      this->_Data[idx] = initialValue;
#ifdef DEBUG_BITUNIT_CONTAINER
    printf("%s:%d done initialization of data in ctor for GCBitUnitArray_moveable _Data[0] @%p\n", __FILE__, __LINE__,
           (void*)&this->_Data[0]);
    printf("%s:%d      final value of idx = %lu\n", __FILE__, __LINE__, idx);
    printf("%s:%d      wrote up to address: %p\n", __FILE__, __LINE__, (void*)&this->_Data[idx]);
#endif
  }
  GCBitUnitArray_moveable(size_t length, bit_array_word* initialContents) : _Length(length) {
    for (size_t i = 0; i < nwords_for_length(length); ++i)
      this->_Data[i] = initialContents[i];
  }
  static size_t nwords_for_length(size_t length) {
    // length/number_of_bit_units_in_word, rounded up
    return (length + number_of_bit_units_in_word - 1) / number_of_bit_units_in_word;
  }
  // sizeof _Data if _Length is the provided value.
  static size_t sizeof_for_length(size_t length) {
    size_t numWords = (length + number_of_bit_units_in_word - 1) / number_of_bit_units_in_word;
    size_t numBytes = numWords * sizeof(bit_array_word);
#ifdef DEBUG_BITUNIT_CONTAINER
    printf("%s:%d length = %lu\n", __FILE__, __LINE__, length);
    printf("%s:%d number_of_bit_units_in_word = %lu\n", __FILE__, __LINE__, number_of_bit_units_in_word);
    printf("%s:%d numWords = %lu\n", __FILE__, __LINE__, numWords);
    printf("%s:%d numBytes = %lu\n", __FILE__, __LINE__, numBytes);
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
      if (result != 0)
        result = ~0;
    } else
      for (size_t i = BitUnitBitWidth; i < BIT_ARRAY_WORD_BITS; i *= 2)
        result |= result << i;
    return result;
  }

public:
  /* Word access */
  bit_array_word& operator[](size_t i) { return this->_Data[i]; };
  const bit_array_word& operator[](size_t i) const { return this->_Data[i]; };

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
    if (Signedp) // template constant
      return (pvalue & magnitude_mask) - (pvalue & sign_mask);
    else
      return pvalue;
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
    size_t offset = (shift_to_0 - (idx % number_of_bit_units_in_word)) * bit_unit_bit_width;
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
    size_t offset = (shift_to_0 - (idx % number_of_bit_units_in_word)) * bit_unit_bit_width;
    bit_array_word mask = bit_unit_mask << offset;
    unsigned char result = (this->_Data[block] & mask) >> offset;
    return sign(result);
  }
  // Get a pointer to the word the given index uses.
  bit_array_word* word(size_t idx) { return &(this->_Data[idx / number_of_bit_units_in_word]); }
  const bit_array_word* word(size_t idx) const { return &(this->_Data[idx / number_of_bit_units_in_word]); }
  // Get the offset value for the given index.
  size_t offset(size_t idx) const { return (shift_to_0 - (idx % number_of_bit_units_in_word)) * bit_unit_bit_width; }

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
    reference(GCBitUnitArray_moveable& arr, size_t idx) noexcept
      : word(arr.word(idx)), offset(arr.offset(idx)) {}
    reference(bit_array_word* w, size_t o) noexcept : word(w), offset(o) {}
    reference(const reference&) = default;
    ~reference() noexcept {}

  private:
    // Get raw bits
    unsigned char raw() const noexcept {
      bit_array_word mask = bit_unit_mask << offset;
      return (*word & mask) >> offset;
    }
    // Set raw bits
    // This is const in that the reference itself is not modified.
    // This "shallow constness" property of this class
    // is needed for algorithms to work
    // (e.g. ranges::reverse_copy in core/array.h)
    void assign(unsigned char x) const noexcept {
      bit_array_word packedVal = (x & bit_unit_mask) << offset;
      bit_array_word mask = ~(bit_unit_mask << offset);
      *word = (*word & mask) | packedVal;
    }

  public:
    // arr[i] = x
    const reference& operator=(value_type x) const noexcept {
      assign(unsign(x));
      return *this;
    }
    // x = arr[i]
    operator value_type() const noexcept { return sign(raw()); }
    // a[i] = b[i], and yes this is required.
    // I don't understand C++ deeply enough, luckily, but a[i] = b[i]
    // seems to be a nop without this.
    const reference& operator=(const reference& x) const noexcept {
      assign(x.raw());
      return *this;
    }
  };
  friend class reference;
  // Like operator[] but for a sub byte unit.
  reference ref(size_t idx) { return reference(*this, idx); }
  value_type ref(size_t idx) const { return bitUnit(idx); }

  class iterator { // a random access iterator in terms of bitunit refs.
  private:
    // NOTE: Keep these in this order so that the defaulted operator<=> is correct.
    bit_array_word* word;
    size_t offset; // reminder: low bits are low indices, so this moves backwards.
  public:
    typedef ptrdiff_t difference_type;
    typedef value_type value_type;
  public:
    // random access iterators must be default constructible.
    // I don't think the resulting object is required to be valid at all though.
    iterator() {};
    iterator(GCBitUnitArray_moveable& arr, size_t idx) noexcept
      : word(arr.word(idx)), offset(arr.offset(idx)) {};
    iterator(bit_array_word* w, size_t o) noexcept : word(w), offset(o) {};
  public:
    reference operator*() const { return reference(word, offset); }
    iterator& operator++() {
      if (offset == 0) {
        word++;
        offset = bits_in_word - bit_unit_bit_width;
      } else offset -= bit_unit_bit_width;
      return *this;
    }
    iterator operator++(int) {
      iterator r(*this);
      ++*this;
      return r;
    }
    iterator& operator--() {
      if (offset == bits_in_word - bit_unit_bit_width) {
        word--;
        offset = 0;
      } else offset += bit_unit_bit_width;
      return *this;
    }
    iterator operator--(int) {
      iterator r(*this);
      --*this;
      return r;
    }
    iterator& operator+=(ptrdiff_t n) {
      word += n / number_of_bit_units_in_word;
      if ((n * bit_unit_bit_width) % bits_in_word > offset) ++word;
      offset = (offset - (n * bit_unit_bit_width)) % bits_in_word; // underflow ok
      return *this;
    }
    iterator& operator-=(ptrdiff_t n) {
      word -= n / number_of_bit_units_in_word;
      if ((n * bit_unit_bit_width) % bits_in_word > bits_in_word - offset) --word;
      offset = (offset + n * bit_unit_bit_width) % bits_in_word;
      return *this;
    }
    iterator operator+(ptrdiff_t n) const {
      iterator r(*this);
      r += n;
      return r;
    }
    friend iterator operator+(difference_type n, const iterator& i) { return i + n; }
    iterator operator-(ptrdiff_t n) const {
      iterator r(*this);
      r -= n;
      return r;
    }
    ptrdiff_t operator-(const iterator& o) const {
      return (word - o.word) * number_of_bit_units_in_word
        + ((ssize_t)o.offset - (ssize_t)offset) / bit_unit_bit_width;
    }
    reference operator[](size_t i) const {
      bit_array_word* nword = word + i / number_of_bit_units_in_word;
      if ((i * bit_unit_bit_width) % bits_in_word > offset) ++nword;
      size_t noffset = (offset - (i * bit_unit_bit_width)) % bits_in_word;
      return reference(nword, noffset);
    }
    // This causes a warning from -Wc++20pcompat for some reason?
    // FIXME: Use this instead of the boring operator definitions below
    //auto operator<=>(const iterator&) const = default;
    bool operator==(const iterator& o) const {
      return word == o.word && offset == o.offset;
    }
    bool operator!=(const iterator& o) const { return !(*this == o); }
    bool operator<(const iterator& o) const {
      return word < o.word || (word == o.word && offset > o.offset);
    }
    bool operator<=(const iterator& o) const {
      return word < o.word || (word == o.word && offset >= o.offset);
    }
    bool operator>(const iterator& o) const { return o < *this; }
    bool operator>=(const iterator& o) const { return o <= *this; }
  }; // class iterator
  class const_iterator { // a random access iterator that returns just bit units.
  private:
    const bit_array_word* word;
    size_t offset;
  public:
    typedef ptrdiff_t difference_type;
    typedef value_type value_type;
  public:
    const_iterator() {};
    const_iterator(GCBitUnitArray_moveable const& arr, size_t idx) noexcept
      : word(arr.word(idx)), offset(arr.offset(idx)) {};
    const_iterator(const bit_array_word* w, size_t o) noexcept
      : word(w), offset(o) {};
  public:
    value_type operator*() const {
      bit_array_word mask = bit_unit_mask << offset;
      return sign((*word & mask) >> offset);
    }
    const_iterator& operator++() {
      if (offset == 0) {
        word++;
        offset = bits_in_word - bit_unit_bit_width;
      } else offset -= bit_unit_bit_width;
      return *this;
    }
    const_iterator operator++(int) {
      const_iterator r(*this);
      ++*this;
      return r;
    }
    const_iterator& operator--() {
      if (offset == bits_in_word - bit_unit_bit_width) {
        word--;
        offset = 0;
      } else offset += bit_unit_bit_width;
      return *this;
    }
    const_iterator operator--(int) {
      const_iterator r(*this);
      --*this;
      return r;
    }
    const_iterator& operator+=(ptrdiff_t n) {
      word += n / number_of_bit_units_in_word;
      if ((n * bit_unit_bit_width) % bits_in_word > offset) ++word;
      offset = (offset - (n * bit_unit_bit_width)) % bits_in_word; // underflow ok
      return *this;
    }
    const_iterator& operator-=(ptrdiff_t n) {
      word -= n / number_of_bit_units_in_word;
      if ((n * bit_unit_bit_width) % bits_in_word > bits_in_word - offset) --word;
      offset = (offset + n * bit_unit_bit_width) % bits_in_word;
      return *this;
    }
    const_iterator operator+(ptrdiff_t n) const {
      const_iterator r(*this);
      r += n;
      return r;
    }
    friend const_iterator operator+(difference_type n, const const_iterator& i) {
      return i + n;
    }
    const_iterator operator-(ptrdiff_t n) const {
      const_iterator r(*this);
      r -= n;
      return r;
    }
    ptrdiff_t operator-(const const_iterator& o) const {
      return (word - o.word) * number_of_bit_units_in_word
        + ((ssize_t)o.offset - (ssize_t)offset) / bit_unit_bit_width;
    }
    value_type operator[](size_t i) const {
      const bit_array_word* nword = word + i / bits_in_word;
      if ((i * bit_unit_bit_width) % bits_in_word > offset) ++nword;
      size_t noffset = (offset - (i * bit_unit_bit_width)) % bits_in_word;
      bit_array_word mask = bit_unit_mask << noffset;
      return sign((*nword & mask) >> noffset);
    }
    //auto operator<=>(const const_iterator&) const = default;
    bool operator==(const const_iterator& o) const {
      return word == o.word && offset == o.offset;
    }
    bool operator!=(const const_iterator& o) const { return !(*this == o); }
    bool operator<(const const_iterator& o) const {
      return word < o.word || (word == o.word && offset > o.offset);
    }
    bool operator<=(const const_iterator& o) const {
      return word < o.word || (word == o.word && offset >= o.offset);
    }
    bool operator>(const const_iterator& o) const { return o < *this; }
    bool operator>=(const const_iterator& o) const { return o <= *this; }
  }; // class const_iterator
  iterator begin() { return iterator(*this, 0); }
  iterator end() { return iterator(*this, _Length); }
  const_iterator begin() const { return const_iterator(*this, 0); }
  const_iterator end() const { return const_iterator(*this, _Length); }
};
} // namespace gctools
