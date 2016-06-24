/*
    File: gcstring.h
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
#ifndef gc_gcstring_H
#define gc_gcstring_H

#include <string.h>

namespace gctools {

/*! A GC aware implementation of std::string */
template <class T = char>
class GCString_moveable : public GCContainer {
public:
  template <class U, typename Allocator>
  friend class GCString;
  typedef T value_type;
  typedef value_type &reference;
  typedef T *iterator;
  typedef T const *const_iterator;

  GCString_moveable(size_t num, size_t e = 0) : _Capacity(num){};
  size_t _Capacity; // Index one beyond the total number of elements allocated
  size_t _End;      // Store the length explicitly
  T _Data[0];       // Store _Capacity numbers of T structs/classes starting here

private:
  GCString_moveable<T>(const GCString_moveable<T> &that);        // disable copy ctor
  GCString_moveable<T> &operator=(const GCString_moveable<T> &); // disable assignment

public:
  value_type *data() { return &this->_Data[0]; };
  size_t size() { return this->_End; };
  size_t capacity() const { return this->_Capacity; };
  value_type &operator[](size_t i) { return this->_Data[i]; };
  const value_type &operator[](size_t i) const { return this->_Data[i]; };
  iterator begin() { return &this->_Data[0]; };
  iterator end() { return &this->_Data[this->_Capacity]; };
  const_iterator begin() const { return &this->_Data[0]; };
  const_iterator end() const { return &this->_Data[this->_Capacity]; };
};

template <class T, typename Allocator = GCStringAllocator<GCString_moveable<T>>>
class GCString {
#ifdef USE_MPS
//        friend GC_RESULT (::obj_scan)(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);
#endif
public:
  typedef Allocator allocator_type;
  typedef T value_type;
  typedef T *pointer_type;
  typedef pointer_type iterator;
  typedef T const *const_iterator;
  typedef T &reference;
  typedef GCString<T, Allocator> my_type;
  typedef GCString_moveable<T> impl_type; // implementation type
  typedef GCString_moveable<T> *pointer_to_moveable;
  typedef gctools::tagged_pointer<GCString_moveable<T>> tagged_pointer_to_moveable;
  static const size_t GCStringPad = 8;
  constexpr static const float GCStringGrow = 2;
  constexpr static const float GCStringShrink = 0.5;

public:
  // Only this instance variable is allowed
  mutable tagged_pointer_to_moveable _Contents;

private:
  /*! This is slow! - it's just for debugging to trap illegal characters
          that might work their way into strings */
  void throwIfIllegalCharacters() const {
    for (const_iterator it = this->begin(); it != this->end(); ++it) {
      if (!(*it == 0 || *it == '\n' || *it == '\t' || (*it >= ' ' && *it < 128))) {
        printf("%s:%d Illegal character [%d/%c] in string at pos %ld from start %p\n", __FILE__, __LINE__, *it, *it, (it - this->begin()), ((void *)(this->begin())));
        THROW_HARD_ERROR(BF("Illegal character [%c] in string at pos %ld from start %p") % *it % (it - this->begin()) % ((void *)(this->begin())));
      }
    }
  }
#if 0
#define THROW_IF_ILLEGAL_CHARACTERS(x) \
  { x->throwIfIllegalCharacters(); };
#else
#define THROW_IF_ILLEGAL_CHARACTERS(x)
#endif

public:
  GCString(const char *chars) : _Contents() {
    size_t sz = strlen(chars);
    this->reserve(sz + GCStringPad);
    strncpy(this->_Contents->data(), chars, sz);
    this->_Contents->_End = sz;
    GCTOOLS_ASSERT(this->_Contents->_End <= this->_Contents->_Capacity);
    THROW_IF_ILLEGAL_CHARACTERS(this);
  };
  /*! Construct and don't initialize contents */
  GCString(int sz) : _Contents() {
    this->reserve(sz + GCStringPad);
    this->_Contents->_End = sz;
    GCTOOLS_ASSERT(this->_Contents->_End <= this->_Contents->_Capacity);
  };
  GCString(const char *chars, int sz) : _Contents() {
    this->reserve(sz + GCStringPad);
    strncpy(this->_Contents->data(), chars, sz);
    this->_Contents->_End = sz;
    GCTOOLS_ASSERT(this->_Contents->_End <= this->_Contents->_Capacity);
    THROW_IF_ILLEGAL_CHARACTERS(this);
  };

  GCString(const string &str) : _Contents() {
    this->reserve(str.size() + GCStringPad);
    strncpy(this->data(), str.data(), str.size());
    this->_Contents->_End = str.size();
    GCTOOLS_ASSERT(this->_Contents->_End <= this->_Contents->_Capacity);
    THROW_IF_ILLEGAL_CHARACTERS(this);
  };

public:
  // Copy Ctor
  GCString<T, Allocator>(const GCString<T, Allocator> &that) 
  {
    if (that._Contents != NULL) {
      allocator_type alloc;
      pointer_to_moveable implAddress = alloc.allocate(that._Contents->_Capacity);
      memcpy(implAddress->_Contents->_Data, that._Contents->_Data, that._Contents->_End * sizeof(value_type));
      implAddress->_Contents->_End = that._Contents->_End;
      this->_Contents = implAddress;
      GCTOOLS_ASSERT(this->_Contents->_Capacity == that._Contents->_Capacity);
      GCTOOLS_ASSERT(this->_Contents->_End <= this->_Contents->_Capacity);
    } else {
      this->_Contents = NULL;
    }
    THROW_IF_ILLEGAL_CHARACTERS(this);
  }

public:
  // Assignment operator must destroy the existing contents
  GCString<T, Allocator> &operator=(const GCString<T, Allocator> &that) {
    if (this != &that) {
      if ((bool)this->_Contents) {
        Allocator alloc;
        gctools::tagged_pointer<GCString_moveable<T>> ptr = this->_Contents;
        this->_Contents.reset_();
        alloc.deallocate(ptr, ptr->_End);
      }
      if ((bool)that._Contents) {
        allocator_type alloc;
        tagged_pointer_to_moveable vec = alloc.allocate(that._Contents->_Capacity);
        memcpy(vec->_Data, that._Contents->_Data, that._Contents->_End * sizeof(value_type));
        vec->_End = that._Contents->_End;
        this->_Contents = vec;
        GCTOOLS_ASSERT(this->_Contents->_Capacity == that._Contents->_Capacity);
        GCTOOLS_ASSERT(this->_Contents->_End <= this->_Contents->_Capacity);
      }
    }
    THROW_IF_ILLEGAL_CHARACTERS(this);
    return *this;
  }

public:
  void swap(my_type &that) {
    tagged_pointer_to_moveable op = that._Contents;
    that._Contents = this->_Contents;
    this->_Contents = op;
    THROW_IF_ILLEGAL_CHARACTERS(this);
  }

  pointer_to_moveable contents() const { return this->_Contents; };

private:
  T &errorEmpty() {
    THROW_HARD_ERROR(BF("GCString had no contents"));
  };
  const T &errorEmpty() const {
    THROW_HARD_ERROR(BF("GCString had no contents"));
  };

public:
  GCString() : _Contents(){};
  ~GCString() {
    if (this->_Contents) {
      Allocator alloc;
      gctools::tagged_pointer<GCString_moveable<T>> ptr = this->_Contents;
      alloc.deallocate(ptr, ptr->_End);
    }
  }

  size_t size() const { return this->_Contents ? this->_Contents->_End : 0; };
  size_t capacity() const { return this->_Contents ? this->_Contents->_Capacity : 0; };

  T &operator[](size_t n) { return this->_Contents ? (*this->_Contents)[n] : this->errorEmpty(); };
  const T &operator[](size_t n) const { return this->_Contents ? (*this->_Contents)[n] : this->errorEmpty(); };

  void reserve(size_t n) const {
    Allocator alloc;
    if (!this->_Contents) {
      tagged_pointer_to_moveable vec;
      size_t newCapacity = (n == 0 ? GCStringPad : n);
      vec = alloc.allocate(newCapacity);
      vec->_End = 0;
      this->_Contents = vec;
      GCTOOLS_ASSERT(newCapacity == this->_Contents->_Capacity);
      GCTOOLS_ASSERT(this->_Contents->_End <= this->_Contents->_Capacity);
      THROW_IF_ILLEGAL_CHARACTERS(this);
      return;
    }
    if (n > this->_Contents->_Capacity) {
      tagged_pointer_to_moveable vec(this->_Contents);
      size_t newCapacity = n;
      vec = alloc.allocate(newCapacity);
      memcpy(vec->_Data, this->_Contents->_Data, this->_Contents->_End * sizeof(value_type));
      vec->_End = this->_Contents->_End;
      //                pointer_to_moveable oldVec(this->_Contents);
      this->_Contents = vec;
      GCTOOLS_ASSERT(newCapacity == this->_Contents->_Capacity);
      GCTOOLS_ASSERT(this->_Contents->_End <= this->_Contents->_Capacity);
      THROW_IF_ILLEGAL_CHARACTERS(this);
      return;
    }
    THROW_IF_ILLEGAL_CHARACTERS(this);
  }

  /*! Resize the vector so that it contains AT LEAST n elements */
  void clear() {
    if (!this->_Contents)
      return;
    this->_Contents->_End = 0;
    // Is it better to reallocate the contents?
    THROW_IF_ILLEGAL_CHARACTERS(this);
  }

  /*! Resize the vector so that it contains AT LEAST n elements */
  void resize(size_t n, const value_type &x = value_type()) {
    Allocator alloc;
    if (!this->_Contents) {
      tagged_pointer_to_moveable vec;
      size_t newCapacity = (n == 0 ? GCStringPad : n * GCStringGrow);
      vec = alloc.allocate(newCapacity);
      // the array at newAddress is undefined - placement new to copy
      for (size_t i(0); i < n; ++i)
        (*vec)[i] = x;
      vec->_End = n;
      this->_Contents = vec;
      GCTOOLS_ASSERT(newCapacity == this->_Contents->_Capacity);
      GCTOOLS_ASSERT(this->_Contents->_End <= this->_Contents->_Capacity);
      THROW_IF_ILLEGAL_CHARACTERS(this);
      return;
    }
    //            size_t oldEnd = this->_Contents->_End;
    //            size_t oldCapacity = this->_Contents->_Capacity;
    if (n == this->_Contents->_End)
      return; // Size isn't changing;
    if (n > this->_Contents->_End) {
      tagged_pointer_to_moveable vec(this->_Contents);
      if (n > this->_Contents->_Capacity) {
        // We need to expand
        size_t newCapacity = n * GCStringGrow;
        vec = alloc.allocate(newCapacity);
        new (&*vec) GCString_moveable<T>(newCapacity);
        memcpy(vec->_Data, this->_Contents->_Data, this->_Contents->_End * sizeof(value_type));
        // fill the new elements with x
        GCTOOLS_ASSERT(vec->_Capacity == newCapacity);
      }
      // Fill from the old end to n with x;
      for (size_t i(this->_Contents->_End); i < n; ++i) {
        (*vec)[i] = x;
      }
      // Set the new length to n
      vec->_End = n;
      if (vec != this->_Contents) {
        // If we created a new vec then wipe out the old
        tagged_pointer_to_moveable oldVec(this->_Contents);
        this->_Contents = vec;
        size_t num = oldVec->_End;
        oldVec->_End = 0;
        alloc.deallocate(oldVec, num);
      }
      return;
    } else if (n < this->_Contents->_Capacity * GCStringShrink) {
      // Handle shrinking by actually shrinking and return shrunk vector
      // We are moving _End down

      GC_LOG(("Add support for shrinking by actually shrinking\n"));
    }
    // I could SPLAT something in the abandoned memory but not now
    this->_Contents->_End = n;
    GCTOOLS_ASSERT(this->_Contents->_End <= this->_Contents->_Capacity);
    THROW_IF_ILLEGAL_CHARACTERS(this);
  }

  string::size_type find_first_of(const string &chars, size_t pos = 0) const {
    for (const_iterator it = this->begin() + pos; it != this->end(); ++it) {
      for (string::const_iterator ci = chars.begin(); ci != chars.end(); ci++) {
        if (*it == *ci) {
          return it - this->begin();
        }
      }
    }
    return string::npos;
  }

  my_type &operator+=(const string &s) {
    this->reserve(this->size() + s.size() + GCStringPad);
    memcpy(this->data() + this->size(), s.data(), s.size());
    this->_Contents->_End += s.size();
    THROW_IF_ILLEGAL_CHARACTERS(this);
    return *this;
  }

  my_type operator+(const string &s) const {
    my_type result(*this);
    result += s;
    THROW_IF_ILLEGAL_CHARACTERS(result);
    return result;
  }

  my_type &operator+=(const my_type &s) {
    this->reserve(this->size() + s.size() + GCStringPad);
    memcpy(this->data() + this->size(), s.data(), s.size() * sizeof(value_type));
    this->_Contents->_End += s.size();
    THROW_IF_ILLEGAL_CHARACTERS(this);
    return *this;
  }

  my_type operator+(const my_type &s) const {
    my_type result(*this);
    result += s;
    THROW_IF_ILLEGAL_CHARACTERS(result);
    return result;
  }

#if 0    
        void push_back(const value_type& x)
        {
            if (!this->_Contents) {
                this->reserve(GCStringPad);
            }
            pointer_to_moveable vec = this->_Contents;
            Allocator alloc;
#ifdef DEBUG_ASSERTS
            if ( this->_Contents->_End > this->_Contents->_Capacity ) {
                THROW_HARD_ERROR(BF("The end should NEVER be beyond the capacity"));
            };
#endif
            if ( strlen(this->_Contents->_End == this->_Contents->_Capacity ) {
                // This is where we grow the Vector
                size_t newCapacity = this->_Contents->_Capacity * GCStringGrow;
                GC_LOG(("Increasing capacity to %zu\n", newCapacity));
#ifdef DEBUG_ASSERTS
                if ( newCapacity > 65536 ) {
                    printf("%s:%d gcvector capacity is larger than 65536\n", __FILE__, __LINE__ );
                }
#endif
                vec = alloc.allocate(newCapacity);
                new (vec) GCString_moveable<T>(newCapacity);
                for ( size_t zi(0); zi<this->_Contents->_End; ++zi ) {                   
                    // the array at newAddress is undefined - placement new to copy
                    alloc.construct(&((*vec)[zi]), (*this->_Contents)[zi]);
                };
                vec->_End = this->_Contents->_End;
            }
            // Placement new in the incoming value of x
            alloc.construct(&((*vec)[this->_Contents->_End]), x);
            ++vec->_End;
            if ( vec != this->_Contents ) {
                // Save the old vector impl
                pointer_to_moveable oldVec(this->_Contents);
                // Replace the old one with the new one in the GVector
                this->_Contents = vec;
                // Deallocate the old one
                size_t num = oldVec->_End;
                oldVec->_End = 0;
                alloc.deallocate(oldVec,num);
            }
        }






        void pop_back() {
#ifdef DEBUG_ASSERTS
            if (!this->_Contents) this->errorEmpty();
#endif
            if ( this->_Contents->_End > 0 ) {
                Allocator alloc;
                // Placement destructor to release the last entry
                alloc.destroy(&(*this->_Contents)[this->_Contents->_End-1]);
                // I could splat stuff in the deallocated memory at (*this)[i] but not now
                // I should have a relocating/resizing version of this as well
                --this->_Contents->_End;
            }
        }


        template <typename...ARGS>
        iterator emplace(const_iterator position, ARGS&&... args)
        {
#ifdef DEBUG_ASSERTS
            if (!this->_Contents) this->errorEmpty();
#endif
            Allocator alloc;
            if (this->_Contents->_End == this->_Contents->_Capacity ) {
                // Must grow the container
                // Save the insertion position relative to the start
                size_t iposition = position - this->begin();
                size_t newCapacity = (this->_Contents->_End+1) * GCStringGrow;
                // Allocate a new vector_moveable
                pointer_to_moveable vec = alloc.allocate(newCapacity);
                new (vec) GCString_moveable<T>(newCapacity);
                // copy elements up to but not including iposition
                for ( size_t zi(0); zi<iposition; ++zi ) alloc.construct(&(*vec)[zi],(*this->_Contents)[zi]);
                // copy in the new element
                alloc.construct(&(*vec)[iposition],std::forward<ARGS>(args)...);
                // Copy elements from old iposition into the new vector_moveable
                for ( size_t zi(iposition); zi<this->_Contents->_End; ++zi ) alloc.construct(&(*vec)[zi+1],(*this->_Contents)[zi]);
                vec->_End = this->_Contents->_End+1;
                pointer_to_moveable oldVec(this->_Contents);
                this->_Contents = vec;
                size_t num = oldVec->_End;
                oldVec->_End = 0;
                alloc.deallocate(oldVec,num);
                return static_cast<iterator>(&((*this->_Contents)[iposition])); // return the new iterator
            }
            // slide the elements from position up to the end one element up
            // Use construct/destruct to deal with objects that have complex constructors/destructors
            for ( iterator zp(this->end()); zp>position; --zp ) {
                alloc.construct(zp,*(zp-1));
                alloc.destroy(zp-1);
            }
            pointer_type ppos = const_cast<pointer_type>(position);
            alloc.construct(ppos,std::forward<ARGS>(args)...);
            ++(this->_Contents->_End);
            return ppos;
        }

        template <typename...ARGS>
        void emplace_back(ARGS&&...args)
        {
            if ( !this->_Contents ) {
                this->reserve(GCStringPad);
            }
            this->emplace(this->end(),std::forward<ARGS>(args)...);
        };

        iterator erase(const_iterator position)
        {
// 0 1 2 3 4 5 6 7 ... N * 
// erase 3 ; position=3 end=N+1
// zp element_of (3 4 5 ... N-1 )
// move 3<4 4<5 5<6 6<7 ... N-2<N-1
// 0 1 2 4 5 6 7
#ifdef DEBUG_ASSERTS
            if (!this->_Contents) this->errorEmpty();
#endif
            Allocator alloc;
            pointer_type zend = (pointer_type)(this->end()-1);
            pointer_type zp = (pointer_type)(position);
            for ( ; zp<zend; ++zp ) {
                alloc.destroy(zp);
                alloc.construct(zp,*(zp+1));
            }
            alloc.destroy(zend);
            --this->_Contents->_End;
            return (iterator)(position);
        }

#endif

  /*! Ensure that there is enough space for the terminal \0,
         append a terminal (value_type)0 to the data and return this->data() */
  T *c_str() {
    this->reserve(this->size() + 1);
    this->operator[](this->size()) = ((value_type)0);
    THROW_IF_ILLEGAL_CHARACTERS(this);
    return this->data();
  }

  const T *c_str() const {
    this->reserve(this->size() + 1);
    T *ptr = const_cast<T *>(&(this->operator[](this->size())));
    *ptr = ((value_type)0);
    THROW_IF_ILLEGAL_CHARACTERS(this);
    return const_cast<const T *>(this->data());
  }

  std::string asStdString() const { return std::string(this->data(), this->size()); };
  pointer_type data() const { return this->_Contents ? this->_Contents->data() : NULL; };

  iterator begin() { return this->_Contents ? &(*this->_Contents)[0] : NULL; }
  iterator end() { return this->_Contents ? &(*this->_Contents)[this->_Contents->_End] : NULL; }

  const_iterator begin() const { return this->_Contents ? &(*this->_Contents)[0] : NULL; }
  const_iterator end() const { return this->_Contents ? &(*this->_Contents)[this->_Contents->_End] : NULL; }
};

typedef gctools::GCString<char, gctools::GCStringAllocator<gctools::GCString_moveable<char>>> gcstring;

} // namespace gctools

#endif
