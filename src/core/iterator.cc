/*
    File: iterator.cc
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
//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/iterator.h>
#include <clasp/core/lisp.h>
#include <clasp/core/wrappers.h>

namespace core {

CL_LAMBDA(it);
CL_DECLARE();
CL_DOCSTRING("iteratorUnsafeElement");
CL_DEFUN T_sp core__iterator_unsafe_element(Iterator_sp it) {
  return it->unsafeElement();
};

CL_LAMBDA(it);
CL_DECLARE();
CL_DOCSTRING("iteratorStep");
CL_DEFUN Iterator_sp core__iterator_step(Iterator_sp it) {
  it->step();
  return it;
};

void Iterator_O::initialize() {
  this->Base::initialize();
}


#if defined(XML_ARCHIVE)
void Iterator_O::archive(ArchiveP node) {
  IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)





};
