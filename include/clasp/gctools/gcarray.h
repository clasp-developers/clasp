/*
    File: gcarray.h
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
#ifndef gc_gcarray_H
#define gc_gcarray_H

namespace gctools {

template <class T>
class GCArray_moveable : public GCContainer {
 public:
 template <class U, typename Allocator>
 friend class GCArray;
 typedef T value_type;
 typedef T *pointer_type;
 typedef value_type &reference;
 typedef value_type container_value_type;
 typedef T *iterator;
 typedef T const *const_iterator;
 size_t _Length; // Index one beyond the total number of elements allocated
 T _Data[0];      // Store _Length numbers of T structs/classes starting here
 // This is the deepest part of the array allocation machinery.
 // The arguments here don't exactly match make-array's, though. Having both is ok here.
 // initialElement is used most of the time.
 // initialContents takes a C array, so it's not directly accessible from Lisp.
 // But it's used for unsafe_subseq (which cl:subseq does use), among other things.
 // Providing both initialElement and initialContents is done when resizing arrays
 // (i.e. allocating new arrays based on old ones).
 GCArray_moveable(size_t length, const T& initialElement, bool initialElementSupplied,
                  size_t initialContentsSize=0, const T* initialContents=NULL) : _Length(length) {
   GCTOOLS_ASSERT(initialContentsSize<=length);
   for ( size_t h(0); h<initialContentsSize; ++h ) {
     this->_Data[h] = initialContents[h];
   }
#if 1
   for (size_t i(initialContentsSize); i<this->_Length; ++i)
     new(&(this->_Data[i])) value_type(initialElement);
#else
   // You can use this to leave arrays uninitialized if there's no :initial-element.
   // I'm leaving it off until we have good reason to know it's worth the attendant weird bugs.
   // (Not that there are any specific known bugs- it's just that it's a bit dangerous.)

   // initialElementSupplied must always be true if T involves pointers, for GC reasons.
   // All code that uses GCArray must ensure this.
   if (initialElementSupplied) {
     for ( size_t i(initialContentsSize); i<this->_Length; ++i ) {
       new(&(this->_Data[i])) value_type(initialElement);
     }
   }
#endif
 }
 public:
 inline size_t size() const { return this->length(); };
 inline size_t length() const { return this->_Length; };
 value_type *data() { return this->_Data; };
 value_type &operator[](size_t i) { return this->_Data[i]; };
 const value_type &operator[](size_t i) const { return this->_Data[i]; };
 iterator begin() { return &this->_Data[0]; };
 iterator end() { return &this->_Data[this->_Length]; };
 const_iterator begin() const { return &this->_Data[0]; };
 const_iterator end() const { return &this->_Data[this->_Length]; };
};

template <class T, typename Allocator>
class GCArray {
#if defined(USE_MPS) && !defined(RUNNING_GC_BUILDER)
  friend GC_RESULT(::obj_scan)(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);
#endif
public:
  // Only this instance variable is allowed
  gctools::tagged_pointer<GCArray_moveable<T>> _Contents;

public:
  typedef Allocator allocator_type;
  typedef T value_type;
  typedef T *pointer_type;
  typedef pointer_type iterator;
  typedef T const *const_iterator;
  typedef T &reference;
  typedef GCArray<T, Allocator> my_type;
  typedef GCArray_moveable<T> impl_type;
  typedef GCArray_moveable<T> *pointer_to_moveable;
  typedef gctools::tagged_pointer<GCArray_moveable<T>> tagged_pointer_to_moveable;

private:
  GCArray<T, Allocator>(const GCArray<T, Allocator> &other);       // disable copy ctor
  GCArray<T, Allocator> &operator=(const GCArray<T, Allocator> &); // disable assignment
public:
  void swap(my_type &other) {
    pointer_to_moveable op = other._Contents;
    other._Contents = this->_Contents;
    this->_Contents = op;
  }

  pointer_to_moveable contents() const { return this->_Contents; };

private:
  T &errorEmpty() {
    throw_hard_error("GCArray had no contents");
  };
  const T &errorEmpty() const {
    throw_hard_error("GCArray had no contents");
  };

public:
 GCArray() : _Contents() {};
  void clear() { this->_Contents = NULL; };

#if 1
  void allocate(const value_type &initial_element, size_t length,bool initElementSupplied=true) {
    GCTOOLS_ASSERTF(!(this->_Contents), "GCArray allocate called and array is already defined");
    allocator_type alloc;
    tagged_pointer_to_moveable implAddress = alloc.allocate_kind(Header_s::StampWtagMtag::make<impl_type>(),length);
    new (&*implAddress) GCArray_moveable<value_type>(length,initial_element,initElementSupplied );
#if 0
    for (size_t i(sizeof...(ARGS)); i < (sizeof...(ARGS)+numExtraArgs); ++i) {
      T *p = &((*implAddress)[i]);
      alloc.construct(p, initialElement);
    }
#endif
    this->_Contents = implAddress;
  }

#else
  template <typename... ARGS>
    void allocate(const value_type &initial_element, size_t length), ARGS &&... args) {
    GCTOOLS_ASSERTF(!(this->_Contents), BF("GCArray allocate called and array is already defined"));
    allocator_type alloc;
    tagged_pointer_to_moveable implAddress = alloc.allocate_kind(Header_s::StampWtagMtag::make<impl_type>(),length);
    new (&*implAddress) GCArray_moveable<value_type>(initial_element, length, std::forward<ARGS>(args)...);
#if 0
    for (size_t i(sizeof...(ARGS)); i < (sizeof...(ARGS)+numExtraArgs); ++i) {
      T *p = &((*implAddress)[i]);
      alloc.construct(p, initialElement);
    }
#endif
    this->_Contents = implAddress;
  }
#endif
  size_t size() const { return this->length(); };
  size_t length() const { return this->_Contents ? this->_Contents->_Length : 0; };
  bool alivep() const { return true; };

  T &operator[](size_t n) { return this->_Contents ? (*this->_Contents)[n] : this->errorEmpty(); };
   const T &operator[](size_t n) const { return this->_Contents ? (*this->_Contents)[n] : this->errorEmpty(); };

  pointer_type data() const { return this->_Contents ? this->_Contents->data() : NULL; };

  iterator begin() { return this->_Contents ? &(*this->_Contents)[0] : NULL; }
  iterator end() { return this->_Contents ? &(*this->_Contents)[this->_Contents->_Length] : NULL; };

  const_iterator begin() const { return this->_Contents ? &(*this->_Contents)[0] : NULL; }
  const_iterator end() const { return this->_Contents ? &(*this->_Contents)[this->_Contents->_Length] : NULL; }


    /*! Resize the vector so that it contains AT LEAST n elements */
  void resize(size_t n, const value_type &x = value_type()) {
    printf("%s:%d How do I resize an array\n", __FILE__, __LINE__ );
  }

};

template <typename Array>
void Array0_dump(const Array &v, const char *head = "") {
  printf("%s Array0@%p _C[%zu]", head, v.contents(), v.length());
  size_t i;
  for (i = 0; i < v.length(); ++i) {
    printf("[%zu]=", i);
    printf("%s ", _rep_(v[i]).c_str());
  }
  printf("\n");
}


 typedef gctools::GCArray<uintptr_t, gctools::GCContainerAllocator<gctools::GCArray_moveable<uintptr_t> > > gcbitvector;


} // namespace gctools

#endif
