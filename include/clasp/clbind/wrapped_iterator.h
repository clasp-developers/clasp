/*
    File: wrapped_iterator.h
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
#ifndef clbind_wrapped_iterator_H
#define clbind_wrapped_iterator_H

#include <clasp/core/foundation.h>
#include <clasp/core/iterator.h>
#include <clasp/core/instance.h>
#include <clasp/clbind/adapter.fwd.h>
#include <clasp/clbind/inheritance.h>

namespace clbind {

template <class IT, typename Policy = reg::null_type>
class Iterator : public core::Iterator_O /*, public gctools::GC_MergeKinds */ {
public:
  typedef core::Iterator_O TemplatedBase;

public:
  IT _Iterator;
  //        End     _end;
public:
  Iterator(IT it /*, End end */) : _Iterator(it) /* , _end(end) */ {};

  core::T_sp unsafeElement() const {
    return translate::to_object<IT>::convert(this->_Iterator);
  }
  size_t templatedSizeof() const { return sizeof(*this); };
  void step() { ++this->_Iterator; };
  bool operator==(core::T_sp other) const {
    if (gctools::smart_ptr<Iterator> io = other.asOrNull<Iterator<IT>>()) {
      return this->_Iterator == io.get()->_Iterator;
    }
    return false;
  }
  bool operator<(core::T_sp other) {
    if (Iterator<IT> *io = other.as<Iterator<IT>>()) {
      return this->_Iterator < io->rawIterator();
    }
    return false;
  }
};
};

template <typename IT, typename Policy>
class gctools::GCKind<clbind::Iterator<IT, Policy>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::Iterator<IT, Policy>::TemplatedBase>::Kind;
};

#endif // clbind_wrapped_iterator
