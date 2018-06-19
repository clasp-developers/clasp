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
class SpecialForm_O : public NamedFunction_O {
  LISP_CLASS(core, CorePkg, SpecialForm_O, "SpecialForm",NamedFunction_O);

public: // virtual functions inherited from Object
  void initialize();
#if defined(XML_ARCHIVE)
  void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
//	string	__repr__() const;

GCPRIVATE: // instance variables
  SpecialFormCallback _fptr;

public:
  static SpecialForm_sp create(Symbol_sp symbol, SpecialFormCallback cb);

public: // initialize
  virtual bool isSpecialForm() { return true; };

  void setf_lambda_list(List_sp lambda_list) {SIMPLE_ERROR_SPRINTF("special-form does not implement setf_lambda_list");};
  LambdaListHandler_sp lambdaListHandler() const { SIMPLE_ERROR_SPRINTF("special-form does not implement lambdaListHandler");} ;
  T_sp setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column) { SIMPLE_ERROR_SPRINTF("special-form does not implement sourcePosInfo"); } ;
  string __repr__() const;
  T_mv evaluate(List_sp args, T_sp environment);
  virtual List_sp declares() const { NOT_APPLICABLE(); };
  virtual T_sp docstring() const { NOT_APPLICABLE(); };
  virtual bool macroP() const { NOT_APPLICABLE(); };
  virtual Symbol_sp getKind() const { NOT_APPLICABLE(); };
  virtual T_sp lambda_list() const { NOT_APPLICABLE(); };
  virtual int sourceFileInfoHandle() const { NOT_APPLICABLE(); };
  virtual T_sp closedEnvironment() const { NOT_APPLICABLE(); };
  virtual void *functionAddress() const { NOT_APPLICABLE(); };
  virtual void set_kind(Symbol_sp k) { NOT_APPLICABLE(); };
  SpecialForm_O(const SpecialForm_O &ss); //!< Copy constructor

 SpecialForm_O(T_sp name) : Base(specialFormDummyEntryPoint,name) {};
  virtual ~SpecialForm_O() {};
};
};
#endif //]
