/*
    File: conditions.cc
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
// This should be the last TURN_DEBUG_off turned off when compiling production code

//
// (C) 2004 Christian E. Schafmeister
//

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <iostream>
#include <fstream>
#include <string>
#include <set>
#include <vector>
#include <clasp/core/common.h>
#include <clasp/core/exceptions.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/bformat.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/str.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/wrappers.h>

#ifdef WIN32
#define vsnprintf _vsnprintf
#endif

#define MAX_DEBUG_CALL_DEPTH 200

namespace core {

EXPOSE_CLASS(core, CandoException_O);

void CandoException_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<CandoException_O>();
}

void CandoException_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, CandoException, "", "", _lisp);
#endif
}

CandoException_sp CandoException_O::create(const string &msg) {
  GC_ALLOCATE(CandoException_O, ce);
  ce->setMessage(msg);
  return ce;
}

CandoException_sp CandoException_O::create(const boost::format &fmt) {
  GC_ALLOCATE(CandoException_O, ce);
  ce->setMessage(fmt.str());
  return ce;
}

Condition::Condition(T_sp cond) {
  this->_ConditionObject = cond;
}

Condition::Condition(const Condition &cc) {
  this->_ConditionObject = cc._ConditionObject;
}

Condition::~Condition() throw() {
  // do nothing
}

T_sp Condition::conditionObject() const {
  HARD_ASSERT(this->_ConditionObject);
  return this->_ConditionObject;
};

void Condition::setConditionObject(T_sp co) {
  this->_ConditionObject = co;
}

#define ARGS_af_makeCondition "(type &rest slot-initializations)"
#define DECL_af_makeCondition ""
#define DOCS_af_makeCondition "make-condition while brcl is booting - replace this once "
T_sp af_makeCondition(T_sp type, List_sp slot_initializations) {
  _G();
  GC_ALLOCATE(CandoException_O, condition);
  Cons_sp all = Cons_O::createList(type, slot_initializations);
  Str_sp msg = gc::As<Str_sp>(af_bformat(_Nil<T_O>(), "%s %s", all));
  condition->setMessage(msg->get());
  return condition;
};

#define ARGS_af_conditionMessage "(c)"
#define DECL_af_conditionMessage ""
#define DOCS_af_conditionMessage "conditionMessage"
string af_conditionMessage(T_sp condition) {
  if (CandoException_sp ce = condition.asOrNull<CandoException_O>()) {
    return ce->message();
  }
  T_sp sout = clasp_make_string_output_stream();
  eval::funcall(cl::_sym_printObject, condition, sout);
  return gc::As<Str_sp>(cl_get_output_stream_string(sout))->get();
}

#if 0
#define ARGS_af_setThrowPosition "(cond file function line)"
#define DECL_af_setThrowPosition ""
#define DOCS_af_setThrowPosition "setThrowPosition"
    void af_setThrowPosition(T_sp cond, Str_sp file, Str_sp function, Fixnum_sp line)
    {_G();
	if ( CandoException_sp ce = cond.asOrNull<CandoException_O>() )
	{
	    string ts = file->get();
	    string fg = function->get();
	    ce->setThrowPosition(ts.c_str(), fg.c_str(), line->get());
	    return ;
	}
	string cs = _rep_(cond);
	printf("%s:%d - Add setThrowPosition for %s\n", __FILE__, __LINE__, cs.c_str());
    };
#endif

void initialize_conditions() {
  SYMBOL_EXPORT_SC_(ClPkg, makeCondition);
  Defun(makeCondition);

  SYMBOL_SC_(CorePkg, conditionMessage);
  Defun(conditionMessage);

  //	SYMBOL_SC_(CorePkg,setThrowPosition);
  //	Defun(setThrowPosition);
};
};
