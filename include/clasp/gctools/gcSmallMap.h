/*
    File: gcSmallMap.h
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
#ifndef gc_gcSmallMap_H
#define gc_gcSmallMap_H

namespace gctools {

struct SmallMapGetError : public std::exception {};

template <class Key, class Value, typename Allocator>
class GCSmallMap : public GCVector<pair<Key, Value>, Allocator> {
public:
  typedef Key key_type;
  typedef Value mapped_type;
  typedef pair<Key, Value> value_type;
  typedef GCVector<value_type, Allocator> Base;
  typedef typename Base::iterator iterator;
  typedef typename Base::const_iterator const_iterator;

public:
  void clear() { this->Base::clear(); };
  const_iterator find(Key k) const {
    iterator it;
    for (it = this->begin(); it != this->end(); ++it) {
      if (it->first == k) {
        return it;
      }
    }
    return it;
  }

  iterator find(Key k) {
    iterator it;
    for (it = this->begin(); it != this->end(); ++it) {
      if (it->first == k) {
        return it;
      }
    }
    return it;
  }

  bool contains(Key k) const {
    return this->find(k) != this->end();
  }

  int count(Key k) const {
    return (this->find(k) != this->end()) ? 1 : 0;
  }

  pair<iterator, bool> insert(const value_type &val) {
    iterator it = this->find(val.first);
    if (it == this->end()) {
      this->push_back(val);
      return make_pair(this->end() - 1, true);
    }
    value_type temp = *this->begin();
    *this->begin() = *it;
    *it = temp;
    return make_pair(this->begin(), false);
  }

  bool remove(const key_type &k) {
    iterator it = this->find(k);
    if (it == this->end()) {
      return false;
    }
    this->Base::erase(it);
    return true;
  }

  mapped_type &get(const key_type &k) {
    iterator it = this->find(k);
    if (it == this->end()) {
      throw SmallMapGetError();
    }
    return it->second;
  }

  const mapped_type &get(const key_type &k) const {
    const_iterator it = this->find(k);
    if (it == this->end()) {
      throw SmallMapGetError();
    }
    return it->second;
  }

  mapped_type &operator[](const key_type &k) {
    return (*((this->insert(std::make_pair(k, mapped_type()))).first)).second;
  }

  mapped_type &operator[](key_type &&k) {
    return (*((this->insert(std::make_pair(k, mapped_type()))).first)).second;
  }

  void set(const key_type &k, const mapped_type &v) {
    (*((this->insert(std::make_pair(k, mapped_type()))).first)).second = v;
  }
};

} // namespace gctools

#endif
