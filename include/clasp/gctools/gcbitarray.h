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

  template <class T>
    class GCBitArray_moveable : public GCContainer {
  public:
    template <class U, typename Allocator>
      friend class GCBitArray;
    typedef T word_type;
    static const size_t BitWidth = sizeof(word_type)/8;
    size_t    _Length; // Index one beyond the total number of elements allocated
    word_type _Data[0];      // Store _Length numbers of bits with multiple bits per T
  public:
  GCBitArray_moveable(uint initialValue, size_t length) : _Length(length) {
      word_type initialFillValue = (initialValue!=0) ? ~0 : 0;
      size_t numWords = (length+(BitWidth-1))/BitWidth;
      for ( size_t i(0); i<this->_Length; ++i ) {
        this->_Data[i] = initialFillValue;
      }
    }
  GCBitArray_moveable() : _Length(0) {};
    word_type &operator[](size_t i) { return this->_Data[i]; };
    const word_type &operator[](size_t i) const { return this->_Data[i]; };
  };



} // namespace gctools

#endif
