/*
    File: cxxClass.cc
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
#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/cxxClass.h>
#include <clasp/core/lisp.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/standardObject.h>
#include <clasp/core/package.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/wrappers.h>

namespace core {

CxxClass_sp CxxClass_O::create(Symbol_sp instanceClassSymbol) {
  _G();
  LOG(BF("Creating CxxClass_O instanceClassSymbol=%d") % instanceClassSymbol);
  GC_ALLOCATE(CxxClass_O, oclass);
  oclass->setName(instanceClassSymbol);
  return ((oclass));
}

CxxClass_sp CxxClass_O::createUncollectable() {
  _G();
  GC_ALLOCATE_UNCOLLECTABLE(CxxClass_O, oclass);
  return ((oclass));
}

CxxClass_O::CxxClass_O() {
}

CxxClass_O::~CxxClass_O() {
}

#if defined(XML_ARCHIVE)
void CxxClass_O::archive(ArchiveP node) {
  IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)

void CxxClass_O::initialize() {
  this->Base::initialize();
  this->initializeSlots(REF_NUMBER_OF_SLOTS_IN_CLASSES);
  //    LOG(BF("For class(%s)@%p handler@%p") % this->static_className() % ((void*)(this)) % this->_InitializationArguments.get() );
}

/* See the description in object.cc Class_O::describe
 */
void CxxClass_O::describe() {
  _G();
  _lisp->print(BF("-------------  Class name: %s") % _rep_(this->name())); //InstanceClassSymbol );
  for (auto cur : this->directSuperclasses()) {
    _lisp->print(BF("Base class: %s") % _rep_((gc::As<Class_sp>(oCar(cur)))->className()));
  }
  _lisp->print(BF("%s") % this->dumpInfo());
  if (this->_creator == NULL) {
    printf("this->_allocator -> NULL\n");
  } else {
    this->_creator->describe();
  }
  _lisp->print(BF("cxxDerivableClassP() -> %d") % this->cxxDerivableClassP());
  _lisp->print(BF("primaryCxxDerivableClassP() -> %d") % this->primaryCxxDerivableClassP());
}

void CxxClass_O::exposeCando(Lisp_sp lisp) {
  class_<CxxClass_O>();
}
void CxxClass_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, CxxClass, "", "", _lisp);
#endif
}

EXPOSE_CLASS(core, CxxClass_O);
};
