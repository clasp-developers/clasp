/*
    File: forwardReferencedClass.cc
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
#include <clasp/core/environment.h>
#include <clasp/core/forwardReferencedClass.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//







void ForwardReferencedClass_O::initialize() {
  this->Base::initialize();
}


CL_DEFUN void core__change_to_standard_class(ForwardReferencedClass_sp orig) {
  // Ok, here I'm going to break a lot of rules.
  // Classes should be instances of Instance_O - I see that now
  // but I implemented them as concrete C++ classes - that makes the unchangeable.
  ForwardReferencedClass_O frc(0);
  StandardClass_O sc(0);
  if (sizeof(ForwardReferencedClass_O) != sizeof(StandardClass_O) ) {
    SIMPLE_ERROR(BF("ForwardReferencedClass_O is not the same size as StandardClass_O and so you cannot change a ForwardReferencedClass_O to a StandardClass_O"));
  }
#if 0
  printf("%s:%d core__change_to_standard_class\n", __FILE__, __LINE__);
  printf("        ForwardReferencedClass_O size -> %" PRu "\n", sizeof(ForwardReferencedClass_O));
  printf("        StandardClass_O size -> %" PRu "\n", sizeof(StandardClass_O));
  printf("        Kind(ForwardReferencedClass_O -> %u\n", gctools::GCStamp<ForwardReferencedClass_O>::Kind);
  printf("        Kind(StandardClass_O -> %u\n", gctools::GCStamp<StandardClass_O>::Kind);
  printf("        vtable(ForwardReferencedClass_O -> %p\n", *(void**)&x);
  printf("        vtable(StandardClass_O -> %p\n", *(void**)&sc);
#endif
  ForwardReferencedClass_O* o = &*orig;
  gctools::Header_s* header = const_cast<gctools::Header_s*>(gctools::header_pointer((void*)o));
  printf("        header -> %p\n", *(void**)header);
  *(void**)&o = *(void**)&sc;
  header->setKind(gctools::GCStamp<StandardClass_O>::Kind);
#if 0
  printf("%s:%d After transform\n",__FILE__,__LINE__);
  printf("        vtable(ForwardReferencedClass_O -> %p\n", *(void**)&o);
  printf("        *header = %p\n", *(void**)header);;
#endif
};
}; /* core */
