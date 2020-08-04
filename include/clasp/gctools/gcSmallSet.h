/*
    File: gcSmallSet.h
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
#ifndef gc_gcSmallSet_H
#define gc_gcSmallSet_H

namespace gctools {

template <class Key, typename Allocator>
class GCSmallSet : public GCVector<Key, Allocator> {
public:
  typedef Key key_type;
  typedef Key value_type;
  typedef GCVector<value_type, Allocator> Base;
  typedef typename Base::iterator iterator;
  typedef typename Base::const_iterator const_iterator;

public:
  const_iterator find(Key k) const {
    iterator it;
    for (it = this->begin(); it != this->end(); ++it) {
      if (*it == k) {
        return it;
      }
    }
    return it;
  }

  iterator find(Key k) {
    iterator it;
    for (it = this->begin(); it != this->end(); ++it) {
      if (*it == k) {
        return it;
      }
    }
    return it;
  }
  int indexOf(Key k) {
    iterator it;
    for (it = this->begin(); it != this->end(); ++it) {
      if (*it == k) {
        return it - this->begin();
      }
    }
    return it - this->begin();
  }

  bool contains(Key k) const {
    return this->find(k) != this->end();
  }

  int count(Key k) const {
    return this->find(k) != this->end() ? 1 : 0;
  }

  pair<iterator, bool> appendNew(const value_type &val) {
    iterator it = this->find(val);
    if (it == this->end()) {
      this->push_back(val);
      return std::make_pair(this->end() - 1, true);
    }
    return std::make_pair(it, false);
  }

  pair<iterator, bool> insert(const value_type &val) {
    return this->appendNew(val);
  }

  size_t erase(const value_type &k) {
    iterator it = this->find(k);
    if (it == this->end()) {
      return 0;
    }
    this->Base::erase(it);
    return 1;
  }
};

} // namespace gctools

#endif
