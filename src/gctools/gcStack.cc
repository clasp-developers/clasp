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

void Frame::dumpFrame(size_t nargs) const {
  printf("%s:%d dumping Frame with %lu elements\n", __FILE__, __LINE__, nargs);
  for (size_t ii = 0; ii < nargs; ++ii) {
    core::T_sp arg((gctools::Tagged)this->_args[ii]);
    printf("%s:%d arg[%lu] -> %s\n", __FILE__, __LINE__, ii, _rep_(arg).c_str());
  }
}

void Frame::debugEmptyFrame(size_t nargs) {
#if DEBUG_FRAME() == 2
  for (size_t ii = 0; ii < nargs; ++ii) {
    (*this)._args[ii] = gctools::tag_unbound<core::T_O*>();
  }
#endif
}

void Frame::checkFrame(size_t idx, size_t nargs) const {
  if (idx != nargs) {
    printf("%s:%d:%s FRAME not filled %lu should be %lu\n", __FILE__, __LINE__, __FUNCTION__, idx, nargs);
    abort();
  }
}

}; // namespace gctools
