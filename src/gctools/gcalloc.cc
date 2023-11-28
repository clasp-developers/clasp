/*
    File: gcalloc.cc
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
#include <clasp/gctools/memoryManagement.h>
#include <clasp/gctools/gcalloc.h>

#ifdef USE_MPS
// header for SNC pool
#include <clasp/mps/code/mpscsnc.h>
#endif

namespace gctools {

/*! Used to signal recursive allocations */
int global_recursive_allocation_counter = 0;

void* malloc_kind_error(uintptr_t expected_kind, uintptr_t kind, uintptr_t size, uintptr_t stmp, void* addr) {
  // print message and abort
  printf("%s:%d Got an unexpected kind: %lu size: %lu stamp: %lu addr: %p   expected: %lu\n", __FILE__, __LINE__, kind, size, stmp,
         addr, expected_kind);
  abort();
}
}; // namespace gctools
