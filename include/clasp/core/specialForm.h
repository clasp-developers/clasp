/*
    File: specialForm.h
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
#ifndef Special_H //[
#define Special_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>

namespace core {

  inline LCC_RETURN specialFormDummyEntryPoint(LCC_ARGS_FUNCALL_ELLIPSIS) {
    SIMPLE_ERROR_SPRINTF("Never call this");
  }
  

SMART(SpecialForm);
class SpecialForm_O : public Function_O {
  LISP_CLASS(core, CorePkg, SpecialForm_O, "SpecialForm",Function_O);

public: // virtual functions inherited from Object
  void initialize();
#if defined(XML_ARCHIVE)
  void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
//	string	__repr__() const;

GCPRIVATE: // instance variables
  SpecialFormCallback _fptr;
  FunctionDescription* _FunctionDescription;
public:
  static SpecialForm_sp create(Symbol_sp symbol, SpecialFormCallback cb);

public: // initialize
  virtual bool isSpecialForm() { return true; };
  virtual FunctionDescription* fdesc() const { return this->_FunctionDescription; };
  virtual void set_fdesc(FunctionDescription* address) { this->_FunctionDescription = address; };

  LambdaListHandler_sp lambdaListHandler() const { SIMPLE_ERROR_SPRINTF("special-form does not implement lambdaListHandler");} ;
  string __repr__() const;
  T_mv evaluate(List_sp args, T_sp environment);
  SpecialForm_O(const SpecialForm_O &ss); //!< Copy constructor

 SpecialForm_O(FunctionDescription* fdesc) : Base(specialFormDummyEntryPoint), _FunctionDescription(fdesc) {};
  virtual ~SpecialForm_O() {};
};
};
#endif //]
