/*
    File: intArray.h
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

#ifndef IntArray_H //[
#define IntArray_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>

namespace core {

SMART(IntArray);
SMART(IntArray);
class IntArray_O : public General_O {
  LISP_CLASS(core, CorePkg, IntArray_O, "IntArray",General_O);

public:
  void initialize();

public:
#if defined(XML_ARCHIVE)
  void archive(ArchiveP node);
#endif // defined(XML_ARCHIVE)
private:
  vector<int> _Ints;

public:
  typedef vector<int>::iterator iterator;

public:
  static IntArray_sp create(uint sz);

public:
  iterator begin() { return this->_Ints.begin(); };
  iterator end() { return this->_Ints.end(); };

  void resize(unsigned sz);
  void clear();

  void append(int val);

  cl_index size() { return this->_Ints.size(); };

  int get(unsigned idx);
  void put(unsigned idx, int val);

  IntArray_O(const IntArray_O &ss); //!< Copy constructor

  DEFAULT_CTOR_DTOR(IntArray_O);
};
};


#endif //]
