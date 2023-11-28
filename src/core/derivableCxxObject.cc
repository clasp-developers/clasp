/*
    File: derivableCxxObject.cc
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
// #define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/derivableCxxObject.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/wrappers.h>

#define READ_DERIVED_STAMP
#include <clasp/llvmo/read-stamp.cc>
#undef READ_DERIVED_STAMP

namespace core {

CL_DEFUN T_sp core__derivable_stamp(T_sp obj) {
  General_O* client_ptr = gctools::untag_general<General_O*>((General_O*)obj.raw_());
  uintptr_t stamp = (uintptr_t)(llvmo::template_read_derived_stamp(client_ptr));
  //  core::clasp_write_string(fmt::format("{}:{}:{} stamp = {}u\n", __FILE__, __LINE__, __FUNCTION__, stamp ));
  T_sp result((gctools::Tagged)stamp);
  return result;
};

}; // namespace core
