#pragma once

/*
    File: gcvector.h
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
namespace core {
bool maybe_demangle(const std::string& fnName, std::string& output);
};
namespace gctools {

template <class T> class GCVector_moveable : public GCContainer {
public:
  template <class U> friend class GCVector;
  typedef GCVector_moveable<T> container_type;
  typedef T value_type;
  typedef value_type container_value_type;
  typedef value_type& reference;

  GCVector_moveable(size_t num, size_t e = 0) : _Capacity(num), _End(e){};
  size_t _Capacity; // Index one beyond the total number of elements allocated
  size_t _End;
  T _Data[0]; // Store _Capacity numbers of T structs/classes starting here

public:
  typedef T* iterator;
  typedef T const* const_iterator;

private:
  GCVector_moveable<T>(const GCVector_moveable<T>& that);       // disable copy ctor
  GCVector_moveable<T>& operator=(const GCVector_moveable<T>&); // disable assignment

public:
  value_type* data() { return &this->_Data[0]; };
  size_t size() { return this->_End; };
  inline void unsafe_set_end(size_t e) { this->_End = e; }
  size_t capacity() const { return this->_Capacity; };
  inline value_type& operator[](size_t i) { return this->_Data[i]; };
  const value_type& operator[](size_t i) const { return this->_Data[i]; };
  iterator begin() { return &this->_Data[0]; };
  iterator end() { return &this->_Data[this->size()]; };
  const_iterator begin() const { return &this->_Data[0]; };
  const_iterator end() const { return &this->_Data[this->size()]; };
};
}; // namespace gctools

namespace gctools {

template <class T> class GCVector {
public:
  typedef GCContainerAllocator<GCVector_moveable<T>> Allocator;
  typedef Allocator allocator_type;
  typedef T value_type;
  typedef T& reference;
  typedef GCVector_moveable<T> impl_type; // implementation type
  typedef GCVector_moveable<T>* pointer_to_moveable;
  typedef gctools::tagged_pointer<GCVector_moveable<T>> tagged_pointer_to_moveable;
  static const size_t GCVectorPad = 8;
  constexpr static const float GCVectorGrow = 2.0;
  constexpr static const float GCVectorShrink = 0.5;
  typedef T* _pointer_type;
  typedef typename impl_type::iterator iterator;
  typedef typename impl_type::const_iterator const_iterator;
  typedef T* pointer;
  typedef const T* const_pointer;
  typedef std::size_t size_type;
  typedef std::ptrdiff_t difference_type;

public:
  // Only this instance variable is allowed
  gctools::tagged_pointer<GCVector_moveable<T>> _Contents;

public:
  inline void unsafe_set_end(size_t e) { this->_Contents->unsafe_set_end(e); }

public:
  // Copy Ctor
  GCVector<T>(const GCVector<T>& that) {
    if (that._Contents) {
      allocator_type alloc;
      tagged_pointer_to_moveable implAddress = alloc.allocate(that._Contents->_Capacity);
      implAddress->_End = 0;
      this->_Contents = implAddress;
      new (&*implAddress) GCVector_moveable<T>(that._Contents->_Capacity);
      for (const_iterator that_it(that.begin()), this_it(this->begin()); that_it != that.end(); ++that_it, ++this_it) {
        alloc.construct(this_it, *that_it);
      }
      this->_Contents->_End = that._Contents->_End;
    } else {
      this->_Contents.reset_();
    }
  }

public:
  // Assignment operator must destroy the existing contents
  GCVector<T>& operator=(const GCVector<T>& that) {
    if (this != &that) {
      if (this->_Contents) {
        Allocator alloc;
        gctools::tagged_pointer<GCVector_moveable<T>> ptr = this->_Contents;
        this->_Contents.reset_();
        alloc.deallocate(ptr, ptr->_End);
      }
      if (that._Contents) {
        allocator_type alloc;
        tagged_pointer_to_moveable implAddress = alloc.allocate(that._Contents->_Capacity);
        new (&*implAddress) GCVector_moveable<T>(that._Contents->_Capacity);
        for (size_t i(0); i < that._Contents->_End; ++i) {
          T* p = &((*implAddress)[i]);
          alloc.construct(p, that._Contents->_Data[i]);
        }
        implAddress->_End = that._Contents->_End;
        this->_Contents = implAddress;
      }
    }
    return *this;
  }

public:
  void swap(GCVector<T>& that) {
    tagged_pointer_to_moveable op = that._Contents;
    that._Contents = this->_Contents;
    this->_Contents = op;
  }

  pointer_to_moveable contents() const { return this->_Contents; };

private:
  T& errorEmpty() { throw_hard_error("GCVector had no contents"); };
  const T& errorEmpty() const { throw_hard_error("GCVector had no contents"); };

public:
  GCVector(bool dummy)
      : _Contents(){
            // Don't GC allocate using this ctor
        };
  GCVector() : _Contents() {
    this->reserve(8); // GC allocate 8 entries
  };
  ~GCVector() {
    if (this->_Contents) {
      Allocator alloc;
      gctools::tagged_pointer<GCVector_moveable<T>> ptr = this->_Contents;
      this->_Contents.reset_();
      alloc.deallocate(ptr, ptr->_End);
    }
  }

  size_t size() const { return this->_Contents ? this->_Contents->_End : 0; };
  size_t capacity() const { return this->_Contents ? this->_Contents->_Capacity : 0; };

  T& operator[](size_t n) {
    GCTOOLS_ASSERT(this->_Contents);
    return (*this->_Contents)[n];
  };
  const T& operator[](size_t n) const {
    GCTOOLS_ASSERT(this->_Contents);
    return (*this->_Contents)[n];
  };

  void push_back(const value_type& x) {
    if (!this->_Contents) {
      this->reserve(GCVectorPad);
    }
    tagged_pointer_to_moveable vec = this->_Contents;
    Allocator alloc;
#ifdef DEBUG_ASSERT
    if (this->_Contents->_End > this->_Contents->_Capacity) {
      printf("%s:%d this@%p this->_Contents -> %p\n", __FILE__, __LINE__, this, this->_Contents.raw_());
      printf("%s:%d this->_Contents->_End = %lu  this->_Contents->_Capacity = %lu\n", __FILE__, __LINE__, this->_Contents->_End,
             this->_Contents->_Capacity);
      throw_hard_error("The end should NEVER be beyond the capacity");
    };
#endif
    size_t contents_end = this->_Contents->_End;
    size_t contents_capacity = this->_Contents->_Capacity;
    if (contents_end == contents_capacity) {
      // This is where we grow the Vector
      size_t new_capacity = contents_capacity * GCVectorGrow;
      GC_LOG(("Increasing capacity to %zu\n", newCapacity));
#ifdef DEBUG_ASSERT
      if (new_capacity > 65536) {
        printf("%s:%d gcvector capacity is larger than 65536\n", __FILE__, __LINE__);
      }
#endif
      vec = alloc.allocate(new_capacity);
      new (&*vec) GCVector_moveable<T>(new_capacity);
      for (size_t zi(0); zi < contents_end; ++zi) {
        // the array at newAddress is undefined - placement new to copy
        alloc.construct(&((*vec)[zi]), (*this->_Contents)[zi]);
      };
      vec->_End = contents_end;
    }
    // Placement new in the incoming value of x
    alloc.construct(&((*vec)[contents_end]), x);
    ++vec->_End;
    if (vec != this->_Contents) {
      // Save the old vector impl
      tagged_pointer_to_moveable oldVec(this->_Contents);
      // Replace the old one with the new one in the GVector
      this->_Contents = vec;
      // Deallocate the old one
      size_t num = oldVec->_End;
      oldVec->_End = 0;
      alloc.deallocate(oldVec, num);
    }
  }

  void reserve(size_t n) {
    Allocator alloc;
    if (!this->_Contents) {
      tagged_pointer_to_moveable vec;
      size_t newCapacity = (n == 0 ? GCVectorPad : n);
      vec = alloc.allocate(newCapacity);
      new (&*vec) GCVector_moveable<T>(newCapacity);
      // the array at newAddress is undefined - placement new to copy
      vec->_End = 0;
      this->_Contents = vec;
      return;
    }
    if (n > this->_Contents->_Capacity) {
      tagged_pointer_to_moveable vec(this->_Contents);
      size_t newCapacity = n;
      vec = alloc.allocate(newCapacity);
      new (&*vec) GCVector_moveable<T>(newCapacity);
      // the array at newAddress is undefined - placement new to copy
      for (size_t zi(0); zi < this->_Contents->_End; ++zi)
        alloc.construct(&(*vec)[zi], (*this->_Contents)[zi]);
      vec->_End = this->_Contents->_End;
      tagged_pointer_to_moveable oldVec(this->_Contents);
      this->_Contents = vec;
      size_t num = oldVec->_End;
      oldVec->_End = 0;
      alloc.deallocate(oldVec, num);
    }
  }

  /*! Resize the vector so that it contains AT LEAST n elements */
  void resize(size_t n, const value_type& x = value_type()) {
    Allocator alloc;
    if (!this->_Contents) {
      tagged_pointer_to_moveable vec;
      size_t newCapacity = (n == 0 ? GCVectorPad : n * GCVectorGrow);
      vec = alloc.allocate(newCapacity);
      new (&*vec) GCVector_moveable<T>(newCapacity);
      // the array at newAddress is undefined - placement new to copy
      for (size_t i(0); i < n; ++i)
        alloc.construct(&(*vec)[i], x);
      vec->_End = n;
      this->_Contents = vec;
      return;
    }
    if (n == this->_Contents->_End)
      return; // Size isn't changing;
    if (n > this->_Contents->_End) {
      tagged_pointer_to_moveable vec(this->_Contents);
      if (n > this->_Contents->_Capacity) {
        size_t newCapacity = n * GCVectorGrow;
        vec = alloc.allocate(newCapacity);
        new (&*vec) GCVector_moveable<T>(newCapacity);
        // the array at newAddress is undefined - placement new to copy
        for (size_t zi(0); zi < this->_Contents->_End; ++zi)
          alloc.construct(&(*vec)[zi], (*this->_Contents)[zi]);
        vec->_End = this->_Contents->_End;
      }
      for (size_t i(this->_Contents->_End); i < n; ++i)
        alloc.construct(&(*vec)[i], x);
      vec->_End = n;
      if (vec != this->_Contents) {
        tagged_pointer_to_moveable oldVec(this->_Contents);
        this->_Contents = vec;
        size_t num = oldVec->_End;
        oldVec->_End = 0;
        alloc.deallocate(oldVec, num);
      }
    }
    // We are moving _End down
    if (n < this->_Contents->_Capacity * GCVectorShrink) { // Handle shrinking by actually shrinking and return shrunk vector
      GC_LOG(("Add support for shrinking by actually shrinking\n"));
    }
    // Placement destructor calls to release stuff past _End
    for (size_t i(n); i < this->_Contents->_End; ++i) {
      GC_LOG(("Placement dtor called on element[%zu]\n", i));
      alloc.destroy(&(*this->_Contents)[i]);
    }
    // Everything after _End is now abandoned
    // I could SPLAT something in the abandoned memory but not now
    this->_Contents->_End = n;
  }

  void pop_back() {
#ifdef DEBUG_ASSERT
    if (!this->_Contents)
      this->errorEmpty();
#endif
    if (this->_Contents->_End > 0) {
      Allocator alloc;
      // Placement destructor to release the last entry
      alloc.destroy(&(*this->_Contents)[this->_Contents->_End - 1]);
      // I could splat stuff in the deallocated memory at (*this)[i] but not now
      // I should have a relocating/resizing version of this as well
      --this->_Contents->_End;
    }
  }

  inline void ensure_initialized() {
    if (!this->_Contents) {
      this->reserve(GCVectorPad);
    }
  }

  template <typename... ARGS> const_iterator emplace(const_iterator position, ARGS&&... args) {
    Allocator alloc;
    if (!this->_Contents)
      this->resize(16, value_type());
    if (this->_Contents->_End == this->_Contents->_Capacity) {
      // Must grow the container
      // Save the insertion position relative to the start
      size_t iposition = position - this->begin();
      size_t newCapacity = (this->_Contents->_End + 1) * GCVectorGrow;
      // Allocate a new vector_moveable
      tagged_pointer_to_moveable vec = alloc.allocate(newCapacity);
      new (&*vec) GCVector_moveable<T>(newCapacity);
      // copy elements up to but not including iposition
      for (size_t zi(0); zi < iposition; ++zi)
        alloc.construct(&(*vec)[zi], (*this->_Contents)[zi]);
      // copy in the new element
      alloc.construct(&(*vec)[iposition], std::forward<ARGS>(args)...);
      // Copy elements from old iposition into the new vector_moveable
      for (size_t zi(iposition); zi < this->_Contents->_End; ++zi)
        alloc.construct(&(*vec)[zi + 1], (*this->_Contents)[zi]);
      vec->_End = this->_Contents->_End + 1;
      tagged_pointer_to_moveable oldVec(this->_Contents);
      this->_Contents = vec;
      size_t num = oldVec->_End;
      oldVec->_End = 0;
      alloc.deallocate(oldVec, num);
      return &(*this->_Contents)[iposition];
    }
    // slide the elements from position up to the end one element up
    // Use construct/destruct to deal with objects that have complex constructors/destructors
    for (iterator zp(this->end()); zp > position; --zp) {
      alloc.construct(&*zp, *(zp - 1));
      alloc.destroy(&*(zp - 1));
    }
    //    _pointer_type ppos = const_cast<_pointer_type>(position);
    alloc.construct(&*position, std::forward<ARGS>(args)...);
    ++(this->_Contents->_End);
    return position;
  }

  template <typename... ARGS> void emplace_back(ARGS&&... args) {
    if (!this->_Contents) {
      this->reserve(GCVectorPad);
    }
    this->emplace(this->end(), std::forward<ARGS>(args)...);
  };

  // 0 1 2 3 4 5 6 7 ... N *
  // erase 3 ; position=3 end=N+1
  // zp element_of (3 4 5 ... N-1 )
  // move 3<4 4<5 5<6 6<7 ... N-2<N-1
  // 0 1 2 4 5 6 7
  iterator erase(iterator position) {
#ifdef DEBUG_ASSERT
    if (!this->_Contents)
      this->errorEmpty();
#endif
    Allocator alloc;
    iterator zend = (this->end() - 1);
    iterator zp = (position);
    for (; zp < zend; ++zp) {
      alloc.destroy(&*zp);
      alloc.construct(&*zp, *(zp + 1));
    }
    alloc.destroy(&*zend);
    --this->_Contents->_End;
    return (iterator)(position);
  }

  void clear() {
    if (!this->_Contents)
      return;
    this->_Contents->_End = 0;
    // Is it better to reallocate the contents?
  }

  //  pointer_type data() const { return this->_Contents ? this->_Contents->data() : NULL; };
  iterator begin() { return this->_Contents ? this->_Contents->begin() : NULL; };
  iterator end() { return this->_Contents ? this->_Contents->end() : NULL; };

  const_iterator begin() const { return this->_Contents ? this->_Contents->begin() : NULL; };
  const_iterator end() const { return this->_Contents ? this->_Contents->end() : NULL; };
};

template <typename Vector> void vector_dump(const Vector& v, const char* head = "") {
  printf("%s vec@%p _C[%zu] _E[%zu] ", head, v.contents(), v.capacity(), v.size());
  size_t i;
  for (i = 0; i < v.capacity(); ++i) {
    if (i == v.size())
      printf("/ ");
    printf("[%zu]=", i);
    v[i].dump();
  }
  if (i == v.size())
    printf("/ ");
  printf("\n");
}

} // namespace gctools
