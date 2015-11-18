/*
    File: containers.h
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
#ifndef gctools_memory_H
#define gctools_memory_H

#include <utility>
namespace gctools {

class GCContainer {
private:
};
};

#include <clasp/gctools/gcvector.h>
#include <clasp/gctools/gcSmallMap.h>
#include <clasp/gctools/gcSmallMultimap.h>
#include <clasp/gctools/gcSmallSet.h>
#include <clasp/gctools/gcarray.h>

namespace gctools {

template <class Vec>
class Vec0_impl {
#if defined(USE_MPS) && !defined(RUNNING_GC_BUILDER)
  friend GC_RESULT(::obj_scan)(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);
#endif
public:
  typedef Vec vector_type;
  typedef typename vector_type::value_type value_type;
  typedef typename vector_type::value_type *pointer_type;
  typedef value_type &reference;
  typedef const value_type &const_reference;
  typedef typename vector_type::iterator iterator;
  typedef typename vector_type::const_iterator const_iterator;

public:
  vector_type _Vector;

public:
  typename Vec::pointer_to_moveable contents() const { return this->_Vector.contents(); };

public:
  void swap(Vec0_impl &other) { this->_Vector.swap(other._Vector); };
  iterator begin() { return this->_Vector.begin(); };
  iterator end() { return this->_Vector.end(); };
  const_iterator begin() const { return this->_Vector.begin(); };
  const_iterator end() const { return this->_Vector.end(); };
  size_t size() const { return this->_Vector.size(); };
  inline void unsafe_set_end(size_t e) { this->_Vector.unsafe_set_end(e); };
  size_t capacity() const { return this->_Vector.capacity(); };
  //  pointer_type data() const { return this->_Vector.data(); };
  ALWAYS_INLINE reference operator[](size_t i) { return this->_Vector[i]; };
  ALWAYS_INLINE const_reference operator[](size_t i) const { return this->_Vector[i]; };
  void resize(size_t n, const value_type &initialElement = value_type()) {
    this->_Vector.resize(n, initialElement);
  };
  void reserve(size_t n) { this->_Vector.reserve(n); };
  void clear() { this->_Vector.clear(); };
  void push_back(const_reference val) { this->_Vector.push_back(val); };
  void pop_back() { this->_Vector.pop_back(); };
  reference back() { return this->_Vector[this->_Vector.size() - 1]; }
  const_reference back() const { return this->_Vector[this->_Vector.size() - 1]; }
  iterator insert(const_iterator position, const value_type &val) { return this->_Vector.emplace(position, val); };
  template <typename... ARGS>
  iterator emplace(const_iterator position, ARGS &&... args) { return this->_Vector.emplace(position, std::forward<ARGS>(args)...); };
  template <typename... ARGS>
  void emplace_back(ARGS &&... args) { this->_Vector.emplace_back(std::forward<ARGS>(args)...); };
  iterator erase(const_iterator position) { return this->_Vector.erase(position); };
};

template <class Arr>
class Array0_impl {
#if defined(USE_MPS) && !defined(RUNNING_GC_BUILDER)
  friend GC_RESULT(::obj_scan)(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);
#endif
public:
  typedef Arr array_type;
  typedef typename array_type::value_type value_type;
  typedef typename array_type::value_type *pointer_type;
  typedef value_type &reference;
  typedef const value_type &const_reference;
  typedef typename array_type::iterator iterator;
  typedef typename array_type::const_iterator const_iterator;

public:
  array_type _Array;

public:
  //        template <typename...ARGS> Array0_impl(size_t numExtraArgs,const value_type& val, ARGS&&...args) : _Array(numExtraArgs,val,std::forward<ARGS>(args)...) {};
  Array0_impl() : _Array(){};

public:
  bool alivep() const { return this->_Array.alivep(); };
  typename Arr::pointer_to_moveable contents() const { return this->_Array.contents(); };

public:
  template <typename... ARGS>
  void allocate(size_t numExtraArgs, const value_type &initialValue, ARGS &&... args) {
    this->_Array.allocate(numExtraArgs, initialValue, std::forward<ARGS>(args)...);
  }

public:
  iterator begin() { return this->_Array.begin(); };
  iterator end() { return this->_Array.end(); };
  const_iterator begin() const { return this->_Array.begin(); };
  const_iterator end() const { return this->_Array.end(); };
  size_t capacity() const { return this->_Array.capacity(); };
  pointer_type data() const { return this->_Array.data(); };
  ALWAYS_INLINE reference operator[](size_t i) { return this->_Array[i]; };
  ALWAYS_INLINE const_reference operator[](size_t i) const { return this->_Array[i]; };
  void clear() { this->_Array.clear(); };
};

#if defined(USE_BOEHM) || defined(USE_MPS)
template <class T>
class Vec0 : public Vec0_impl<GCVector<T, GCContainerAllocator<GCVector_moveable<T>>>> {
public:
  typedef Vec0_impl<GCVector<T, GCContainerAllocator<GCVector_moveable<T>>>> Base;
  Vec0() : Base(){};
};

template <class K, class V>
class SmallMap : public GCSmallMap<K, V, GCContainerAllocator<GCVector_moveable<pair<K, V>>>> {
public:
  typedef GCSmallMap<K, V, GCContainerAllocator<GCVector_moveable<pair<K, V>>>> Base;
  SmallMap() : Base(){};
};

template <class K, class V, class Compare>
class SmallMultimap : public GCSmallMultimap<K, V, Compare, GCContainerAllocator<GCVector_moveable<pair<K, V>>>> {
public:
  typedef GCSmallMultimap<K, V, Compare, GCContainerAllocator<GCVector_moveable<pair<K, V>>>> Base;

public:
  void insert2(K key, V value) {
    pair<K, V> key_value(key, value);
    this->insert(key_value);
  }

  SmallMultimap() : Base(){};
};

template <class K>
class SmallOrderedSet : public GCSmallSet<K, GCContainerAllocator<GCVector_moveable<K>>> {
public:
  typedef GCSmallSet<K, GCContainerAllocator<GCVector_moveable<K>>> Base;
  SmallOrderedSet() : Base(){};
};

template <class T>
class Array0 : public Array0_impl<GCArray<T, GCContainerAllocator<GCArray_moveable<T>>>> {
public:
  typedef Array0_impl<GCArray<T, GCContainerAllocator<GCArray_moveable<T>>>> Base;
  //        template <typename...ARGS> Array0(size_t numExtraArgs,const T& val, ARGS&&...args) : Base(numExtraArgs,val,std::forward<ARGS>(args)...) {};
  Array0() : Base(){};
};

//
// Use these for ActivationFrames to distinguish them from GCArray
//
template <class T>
class Frame0 : public Array0_impl<GCArray<T, GCContainerAllocator<GCArray_moveable<T>>>> {
public:
  typedef Array0_impl<GCArray<T, GCContainerAllocator<GCArray_moveable<T>>>> Base;
  //        template <typename...ARGS> Frame0(size_t numExtraArgs,const T& val, ARGS&&...args) : Base(numExtraArgs,val,std::forward<ARGS>(args)...) {};
  Frame0() : Base(){};
};
#endif

#ifdef USE_MPS_OLD
template <class T>
class Vec0 : public Vec0_impl<GCVector<T, GCContainerAllocator_mps<GCVector_moveable<T>>>> {
public:
  typedef Vec0_impl<GCVector<T, GCContainerAllocator_mps<GCVector_moveable<T>>>> Base;
  Vec0() : Base(){};
};

template <class T>
class Array0 : public Array0_impl<GCArray<T, GCContainerAllocator_mps<GCArray_moveable<T>>>> {
public:
  typedef Array0_impl<GCArray<T, GCContainerAllocator_mps<GCArray_moveable<T>>>> Base;
  //        template <typename...ARGS> Array0(size_t numExtraArgs,const T& val, ARGS&&...args) : Base(numExtraArgs,val,std::forward<ARGS>(args)...) {};
  Array0() : Base(){};
};

//
// Use these for ActivationFrames to distinguish them from GCArray
//
template <class T>
class Frame0 : public Array0_impl<GCArray<T, GCContainerAllocator_mps<GCArray_moveable<T>>>> {
public:
  typedef Array0_impl<GCArray<T, GCContainerAllocator_mps<GCArray_moveable<T>>>> Base;
  //        template <typename...ARGS> Frame0(size_t numExtraArgs,const T& val, ARGS&&...args) : Base(numExtraArgs,val,std::forward<ARGS>(args)...) {};
  Frame0() : Base(){};
};
#endif // USE_MPS

}; // namespace gctools
#endif
