/*
    File: iterator.cc
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

#include <clasp/core/common.h>
#include <clasp/core/iterator.h>
#include <clasp/core/lisp.h>
#include <clasp/core/wrappers.h>

namespace core {

#define ARGS_af_iteratorUnsafeElement "(it)"
#define DECL_af_iteratorUnsafeElement ""
#define DOCS_af_iteratorUnsafeElement "iteratorUnsafeElement"
T_sp af_iteratorUnsafeElement(Iterator_sp it) {
  _G();
  return it->unsafeElement();
};

#define ARGS_af_iteratorStep "(it)"
#define DECL_af_iteratorStep ""
#define DOCS_af_iteratorStep "iteratorStep"
Iterator_sp af_iteratorStep(Iterator_sp it) {
  _G();
  it->step();
  return it;
};

void Iterator_O::initialize() {
  this->Base::initialize();
}

#if defined(XML_ARCHIVE)
void Iterator_O::archive(ArchiveP node) {
  IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)

void Iterator_O::exposeCando(Lisp_sp lisp) {
  class_<Iterator_O>()
      .def("core:begin", &Iterator_O::first)
      .def("next", &Iterator_O::next)
      .def("isDone", &Iterator_O::isDone)
      .def("notDone", &Iterator_O::notDone)
      .def("currentObject", &Iterator_O::currentObject);
  //	def("create_Iterator",&Iterator_O::create);
  Defun(iteratorStep);
  Defun(iteratorUnsafeElement);
}

void Iterator_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON //[
  PYTHON_CLASS(CorePkg, Iterator, "", "", _lisp)
      .def("core:begin", &Iterator_O::first)
      .def("next", &Iterator_O::next)
      .def("isDone", &Iterator_O::isDone)
      .def("notDone", &Iterator_O::notDone)
      .def("currentObject", &Iterator_O::currentObject);
#endif //]
}

EXPOSE_CLASS(core, Iterator_O);
};
