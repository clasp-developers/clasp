/*
    File: creator.cc
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
#include <clasp/core/lisp.h>
#include <clasp/core/str.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/standardObject.h>
#include <clasp/core/package.h>
//#include "debugger.h"
#include <clasp/core/iterator.h>
#include <clasp/core/designators.h>
#include <clasp/core/primitives.h>
#include <clasp/core/instance.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/lambdaListHandler.h>
//#i n c l u d e "environmentDependent.h"
#include <clasp/core/environment.h>
#include <clasp/core/evaluator.h>
// to avoid Generic to_object include headers here
#include <clasp/core/wrappers.h>



namespace core {
T_sp InstanceCreator_O::creator_allocate() {
  // TODO: I think I need to replace gctools::GCKind<Instance_O>::Kind with the Kind/Stamp of the class
  Instance_sp instance = gctools::GCObjectAllocator<Instance_O>::allocate_kind(this->_class->_instance_stamp,gctools::GCKind<Instance_O>::Kind, gctools::sizeof_with_header<Instance_O>());
  return instance;
//      GC_ALLOCATE(Instance_O, output);
//      return output;
    };


};
