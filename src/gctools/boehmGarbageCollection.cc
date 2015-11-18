/*
    File: boehmGarbageCollection.cc
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
#include <clasp/core/object.h>
#include <clasp/core/numbers.h>
#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/gctools/boehmGarbageCollection.h>

namespace gctools {

#ifdef USE_BOEHM_MEMORY_MARKER
int globalBoehmMarker = 0;
#endif

void rawHeaderDescribe(uintptr_t *rawheaderP) {
  Header_s *headerP = reinterpret_cast<Header_s *>(rawheaderP);
  printf("  0x%p : Kind: 0x%p  vtable: 0x%p\n", headerP, *headerP, *(headerP + 1));
  gctools::GCKindEnum kind = headerP->kind();
  printf(" Kind tag - kind: %d\n", kind);
  fflush(stdout);
};

void headerDescribe(core::T_O *taggedClient) {
  if (tagged_generalp(taggedClient) || tagged_consp(taggedClient)) {
    printf("%s:%d  GC managed object - describing header\n", __FILE__, __LINE__);
    // Currently this assumes that Conses and General objects share the same header
    // this may not be true in the future
    // conses may be moved into a separate pool and dealt with in a different way
    uintptr_t *headerP;
    if (tagged_generalp(taggedClient)) {
      headerP = reinterpret_cast<uintptr_t *>(ClientPtrToBasePtr(untag_general(taggedClient)));
    } else {
      headerP = reinterpret_cast<uintptr_t *>(ClientPtrToBasePtr(untag_cons(taggedClient)));
    }
    rawHeaderDescribe(headerP);
  } else {
    printf("%s:%d Not a tagged pointer - might be immediate value\n", __FILE__, __LINE__);
  };
};
};
