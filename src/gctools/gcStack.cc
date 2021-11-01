/*
    File: gcStack.cc
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
#include <clasp/core/foundation.h>
namespace gctools {


void Frame::dump() const {
  size_t numElements = this->number_of_arguments();
  printf("%s:%d dumping Frame with %lu elements\n", __FILE__, __LINE__, numElements);
  core::Closure_sp closure((gctools::Tagged)this->data0()[0]);
  printf("%s:%d closure %s\n", __FILE__, __LINE__, _rep_(closure).c_str());
  for ( size_t ii=0; ii<numElements; ++ii ) {
    core::T_sp arg((gctools::Tagged)(*this)[ii]);
    printf("%s:%d arg[%lu] -> %s\n", __FILE__, __LINE__, ii, _rep_(arg).c_str());
  }
}

};
