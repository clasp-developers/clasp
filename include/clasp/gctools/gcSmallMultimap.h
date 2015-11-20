/*
    File: gcSmallMultimap.h
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
#ifndef gc_gcSmallMultimap_H
#define gc_gcSmallMultimap_H

namespace gctools {

struct SmallMultimapGetError : public std::exception {};

template <class Key, class Value, class Compare, typename Allocator>
class GCSmallMultimap : public GCVector<pair<Key, Value>, Allocator> {
public:
  typedef Compare key_compare;
  typedef Key key_type;
  typedef Value mapped_type;
  typedef pair<Key, Value> value_type;
  typedef GCVector<value_type, Allocator> Base;
  typedef typename Base::iterator iterator;
  typedef typename Base::const_iterator const_iterator;

public:
  pair<iterator, iterator> equal_range(Key k) {
    for (auto it = this->begin(); it != this->end(); ++it) {
      int order = key_compare::order(k, it->first);
      if (order == -1) { // hit greater, never found equal
        return pair<iterator, iterator>(this->end(), this->end());
      }
      if (order == 0) { // equal - find end of equal range
        for (auto jt = it + 1; jt != this->end(); ++jt) {
          int jorder = key_compare::order(k, jt->first);
          if (jorder != 0) {
            return pair<iterator, iterator>(it, jt);
          }
        }
        return pair<iterator, iterator>(it, this->end());
      }
    }
    // never found greater
    return pair<iterator, iterator>(this->end(), this->end());
  }

  const_iterator find(Key k) const {
    const_iterator it;
    for (it = this->begin(); it != this->end(); ++it) {
      int order = key_compare::order(k, it->first);
      if (order == 0)
        return it;
      if (order == 1)
        return this->end();
    }
    return it;
  }

  iterator find(Key k) {
    iterator it;
    for (it = this->begin(); it != this->end(); ++it) {
      int order = key_compare::order(k, it->first);
      if (order == 0)
        return it;
      if (order == 1)
        return this->end();
    }
    return it;
  }

  bool contains(Key k) const {
    return (this->find(k) != this->end());
  }

  int count(Key k) const {
    pair<iterator, iterator> range = this->equal_range(k);
    return range->second - range->first;
  }

  pair<iterator, bool> insert(const value_type &val) {
    iterator it;
    if (!this->_Contents) {
      this->reserve(4);
    }
    for (it = this->begin(); it != this->end(); ++it) {
      int order = key_compare::order(val.first, it->first);
      if (order != 1) {
        it = this->emplace(it, val);
        return pair<iterator, bool>(it, true);
      }
    }
    it = this->emplace(it, val);
    return pair<iterator, bool>(it, true);
  }

  mapped_type &get(const key_type &k) {
    iterator it = this->find(k);
    if (it == this->end()) {
      throw SmallMultimapGetError();
    }
    return it->second;
  }

  const mapped_type &get(const key_type &k) const {
    const_iterator it = this->find(k);
    if (it == this->end()) {
      throw SmallMultimapGetError();
    }
    return it->second;
  }
};

} // namespace gctools

#endif
