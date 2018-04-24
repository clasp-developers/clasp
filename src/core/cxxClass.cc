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
//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/package.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/wrappers.h>

namespace core {

CxxClass_sp CxxInstance_O::create(Symbol_sp instanceClassSymbol) {
  LOG(BF("Creating CxxInstance_O instanceClassSymbol=%d") % instanceClassSymbol);
  GC_ALLOCATE(CxxInstance_O, oclass);
  oclass->setName(instanceClassSymbol);
  return ((oclass));
}

CxxClass_sp CxxInstance_O::createUncollectable(gctools::Stamp is) {
  GC_ALLOCATE_UNCOLLECTABLE(CxxInstance_O, oclass, is);
  return ((oclass));
}

#if defined(XML_ARCHIVE)
void CxxInstance_O::archive(ArchiveP node) {
  IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)

void CxxInstance_O::initialize() {
  DEPRECATED();
  this->Base::initialize();
  this->initializeSlots(REF_NUMBER_OF_SLOTS_IN_CLASSES);
  this->initializeClassSlots();
  //    LOG(BF("For class(%s)@%p handler@%p") % this->static_className() % ((void*)(this)) % this->_InitializationArguments.get() );
}

/* See the description in object.cc Instance_O::describe
 */
void CxxInstance_O::describe(T_sp stream) {
  stringstream ss;
  ss << (BF("-------------  Class name: %s") % _rep_(this->name())).str(); //InstanceClassSymbol );
  for (auto cur : this->directSuperclasses()) {
    ss << (BF("Base class: %s") % _rep_((gc::As<Class_sp>(oCar(cur)))->_className())).str();
  }
  ss << (BF("%s") % this->dumpInfo()).str();
  clasp_write_string(ss.str(), stream);
#if 0
  if (!this->_theCreator) {
    stringstream ss2;
    ss2 << (BF("this->_allocator -> NULL\n")).str();
    clasp_write_string(ss2.str(),stream);
  } else {
    this->_theCreator->describe(stream);
  }
#endif
  stringstream sr;
  sr << (BF("cxxDerivableClassP() -> %d") % this->cxxDerivableClassP()).str();
  sr << (BF("primaryCxxDerivableClassP() -> %d") % this->primaryCxxDerivableClassP()).str();
  clasp_write_string(sr.str(), stream);
}




};
