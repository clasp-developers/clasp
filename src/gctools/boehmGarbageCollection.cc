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
  printf("  0x%p : Kind: 0x%zu\n", headerP, headerP->Kind);
  gctools::GCKindEnum kind = headerP->kind();
  printf(" Kind tag - kind: %d\n", kind);
  fflush(stdout);
};

};

namespace gctools  {
void boehm_register_roots(void* root_address, size_t num_roots)
{
  core::T_sp* dest = reinterpret_cast<core::T_sp*>(GC_MALLOC_UNCOLLECTABLE(sizeof(core::T_sp)*num_roots));
  std::memcpy(dest,root_address,sizeof(core::T_sp)*num_roots);
}

}


extern "C" {
void client_describe(void *taggedClient) {
  if (gctools::tagged_generalp(taggedClient) || gctools::tagged_consp(taggedClient)) {
    printf("%s:%d  GC managed object - describing header\n", __FILE__, __LINE__);
    // Currently this assumes that Conses and General objects share the same header
    // this may not be true in the future
    // conses may be moved into a separate pool and dealt with in a different way
    uintptr_t *headerP;
    if (gctools::tagged_generalp(taggedClient)) {
      headerP = reinterpret_cast<uintptr_t *>(gctools::ClientPtrToBasePtr(gctools::untag_general(taggedClient)));
      gctools::rawHeaderDescribe(headerP);
    } else if (gctools::tagged_consp(taggedClient)) {
      printf("%s:%d A cons pointer\n", __FILE__, __LINE__ );
    }
  } else {
    printf("%s:%d Not a tagged pointer - might be immediate value\n", __FILE__, __LINE__);
  };
};


  
void client_validate(void *taggedClient)
{
}

};
