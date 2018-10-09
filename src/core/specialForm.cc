/*
    File: specialForm.cc
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
#include <clasp/core/specialForm.h>
#include <clasp/core/lisp.h>
//#include "debugger.h"
// last include is wrappers.h
#include <clasp/core/wrappers.h>

namespace core {




SpecialForm_sp SpecialForm_O::create(Symbol_sp symbol, SpecialFormCallback fptr) {
  FunctionDescription* fdesc = makeFunctionDescription(symbol);
  SpecialForm_sp sf = gctools::GC<SpecialForm_O>::allocate(fdesc);
  sf->_fptr = fptr;
  return sf;
}

T_mv SpecialForm_O::evaluate(List_sp args, T_sp environment) {
  _OF();
  ASSERTP(this->_fptr != NULL, "Functoid can not be NULL");
  return (this->_fptr)(args, environment);
}

void SpecialForm_O::initialize() {
  this->Base::initialize();
}

#if defined(XML_ARCHIVE)
void SpecialForm_O::archiveBase(ArchiveP node) {
  this->Base::archiveBase(node);
  IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)

string SpecialForm_O::__repr__() const {
  return this->functionName().as<Symbol_O>()->fullName();
}


};
