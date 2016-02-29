/*
    File: gc_interface.h
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
#ifndef GC_INTERFACE_H
#define GC_INTERFACE_H
#include <clasp/core/foundation.h>

//
// All class forward declarations
//
namespace core {
class T_O;
class WrappedPointer_O;
class Functoid;
class Creator;
class Iterator_O;
class SequenceStepper;
};
namespace asttooling {
namespace internal {
class MatcherDescriptor;
};
};
namespace clbind {
class ConstructorCreator;
};




//#define GC_INTERFACE_FORWARD
//#include <project_headers.h>
//#undef GC_INTERFACE_FORWARD

#ifdef USE_MPS
#ifndef RUNNING_GC_BUILDER // when running the static analyzer - don't include the following
#define DECLARE_FORWARDS
#include "clasp_gc.cc"
#undef DECLARE_FORWARDS
#endif // ifndef RUNNING_GC_BUILDER
#endif
#ifdef USE_BOEHM
#ifdef USE_CXX_DYNAMIC_CAST
// nothing
#else
#define DECLARE_FORWARDS
#include "clasp_gc.cc"
#undef DECLARE_FORWARDS
#endif
#endif

namespace gctools {

#ifdef USE_MPS
#ifndef RUNNING_GC_BUILDER // when running the static analyzer - don't include the following
#define GC_KIND_SELECTORS
#include "clasp_gc.cc"
#undef GC_KIND_SELECTORS
#endif // ifndef RUNNING_GC_BUILDER
#endif
#ifdef USE_BOEHM
#ifdef USE_CXX_DYNAMIC_CAST
// Nothing
#else
#define GC_KIND_SELECTORS
#include "clasp_gc.cc"
#undef GC_KIND_SELECTORS
#endif
#endif
};

#include <clasp/gctools/other_tagged_casts.h>

extern "C" {
const char *obj_name(gctools::kind_t kind);
extern void obj_dump_base(void *base);
extern void obj_deallocate_unmanaged_instance(gctools::smart_ptr<core::T_O> obj);

extern int global_symbol_count;
extern gctools::smart_ptr<core::Symbol_O> global_symbols[];

extern void client_validate_internal(core::T_O* tagged_client);
extern void client_validate_recursive(core::T_O* tagged_client, std::set<core::T_O*> seen );
};

void initialize_clasp();

void initialize_functions();
void initialize_source_info();
void initialize_classes_and_methods();


#endif
