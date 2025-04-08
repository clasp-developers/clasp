#pragma once

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

#include <utility>
namespace gctools {

class GCContainer {
private:
};
}; // namespace gctools

#include <clasp/gctools/gcvector.h>
#include <clasp/gctools/gcSmallMap.h>
#include <clasp/gctools/gcSmallMultimap.h>
#include <clasp/gctools/gcSmallSet.h>
#include <clasp/gctools/gcarray.h>
#include <clasp/gctools/gcbitarray.h>

namespace gctools {

template <class T> class Vec0 {
public:
  typedef GCVector<T> vector_type;
  typedef typename vector_type::value_type value_type;
  typedef typename vector_type::value_type* pointer_type;
  typedef value_type& reference;
  typedef const value_type& const_reference;
  typedef typename vector_type::iterator iterator;
  typedef typename vector_type::const_iterator const_iterator;
  typedef typename vector_type::difference_type difference_type;
  typedef typename vector_type::size_type size_type;
  typedef typename vector_type::pointer pointer;
  typedef typename vector_type::const_pointer const_pointer;

public:
  vector_type _Vector;
  Vec0(bool dummy) : _Vector(dummy){}; // don't allocate GC memory ctor
  Vec0() = default;

public:
  typename vector_type::pointer_to_moveable contents() const { return this->_Vector.contents(); };

public:
  void swap(Vec0& other) { this->_Vector.swap(other._Vector); };
  iterator begin() { return this->_Vector.begin(); };
  iterator end() { return this->_Vector.end(); };
  const_iterator begin() const { return this->_Vector.begin(); };
  const_iterator end() const { return this->_Vector.end(); };
  size_t size() const { return this->_Vector.size(); };
  bool empty() const { return this->_Vector.size() == 0; };
  inline void unsafe_set_end(size_t e) { this->_Vector.unsafe_set_end(e); };
  void ensure_initialized() { return this->_Vector.ensure_initialized(); };
  size_t capacity() const { return this->_Vector.capacity(); };
  size_t max_size() const { return ~(size_t)0; };
  pointer_type data() const { return this->_Vector._Contents->data(); };
  inline void operator=(const Vec0& other) { this->_Vector = other._Vector; }
  inline reference operator[](size_t i) { return this->_Vector[i]; };
  inline const_reference operator[](size_t i) const { return this->_Vector[i]; };
  void resize(size_t n, const value_type& initialElement = value_type()) { this->_Vector.resize(n, initialElement); };
  void reserve(size_t n) { this->_Vector.reserve(n); };
  void assign(size_t count, const value_type& value) {
    this->resize(count);
    for (size_t zz = 0; zz < count; zz++)
      this->_Vector[zz] = value;
  }
  void assign(const_iterator begin, const_iterator end) {
    size_t count = std::distance(begin, end);
    this->resize(count);
    for (size_t zz = 0; zz < count; zz++)
      this->_Vector[zz] = begin[zz];
  }
  void clear() { this->_Vector.clear(); };
  void push_back(const_reference val) { this->_Vector.push_back(val); };
  void pop_back() { this->_Vector.pop_back(); };
  reference front() { return this->_Vector[0]; }
  const_reference front() const { return this->_Vector[0]; }
  reference back() { return this->_Vector[this->_Vector.size() - 1]; }
  const_reference back() const { return this->_Vector[this->_Vector.size() - 1]; }
  iterator insert(const_iterator position, const value_type& val) {
    return const_cast<iterator>(this->_Vector.emplace(position, val));
  };
  template <typename... ARGS> iterator emplace(const_iterator position, ARGS&&... args) {
    return this->_Vector.emplace(position, std::forward<ARGS>(args)...);
  };
  template <typename... ARGS> void emplace_back(ARGS&&... args) { this->_Vector.emplace_back(std::forward<ARGS>(args)...); };
  iterator erase(iterator position) { return this->_Vector.erase(position); };
};

template <class Arr> class Array0_impl {
public:
  typedef Arr array_type;
  typedef typename array_type::value_type value_type;
  typedef typename array_type::value_type* pointer_type;
  typedef value_type& reference;
  typedef const value_type& const_reference;
  typedef typename array_type::iterator iterator;
  typedef typename array_type::const_iterator const_iterator;

public:
  array_type _Array;

public:
  //        template <typename...ARGS> Array0_impl(size_t numExtraArgs,const value_type& val, ARGS&&...args) :
  //        _Array(numExtraArgs,val,std::forward<ARGS>(args)...) {};
  Array0_impl() : _Array(){};

public:
  bool alivep() const { return this->_Array.alivep(); };
  typename Arr::pointer_to_moveable contents() const { return this->_Array.contents(); };

public:
  template <typename... ARGS> void allocate(size_t numExtraArgs, const value_type& initialValue, ARGS&&... args) {
    this->_Array.allocate(numExtraArgs, initialValue, std::forward<ARGS>(args)...);
  }

public:
  iterator begin() { return this->_Array.begin(); };
  iterator end() { return this->_Array.end(); };
  const_iterator begin() const { return this->_Array.begin(); };
  const_iterator end() const { return this->_Array.end(); };
  size_t capacity() const { return this->_Array.capacity(); };
  pointer_type data() const { return this->_Array.data(); };
  inline reference operator[](size_t i) { return this->_Array[i]; };
  inline const_reference operator[](size_t i) const { return this->_Array[i]; };
  void clear() { this->_Array.clear(); };
};

}; // namespace gctools

namespace gctools {
template <class T> class Vec0_uncopyable : public Vec0<T> {
public:
  typedef Vec0<T> Base;
  Vec0_uncopyable() : Base(){};
  Vec0_uncopyable(const Vec0_uncopyable<T>& orig) : Base(){};
};

}; // namespace gctools
