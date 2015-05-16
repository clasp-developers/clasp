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
};
namespace clbind {
class ConstructorCreator;
};

#ifndef RUNNING_GC_BUILDER // when running the static analyzer - don't include the following
#ifdef USE_MPS
#define DECLARE_FORWARDS
#include GARBAGE_COLLECTION_INCLUDE
#undef DECLARE_FORWARDS
#endif
#endif // ifndef RUNNING_GC_BUILDER

namespace gctools {

#ifndef RUNNING_GC_BUILDER // when running the static analyzer - don't include the following
#ifdef USE_MPS
#define GC_KIND_SELECTORS
#include GARBAGE_COLLECTION_INCLUDE
#undef GC_KIND_SELECTORS
#endif
#endif // ifndef RUNNING_GC_BUILDER
};
#endif
