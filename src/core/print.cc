/*
    File: print.cc
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
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/str.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/primitives.h>
#include <clasp/core/write_object.h>
#include <clasp/core/arguments.h>
#include <clasp/core/wrappers.h>

namespace core {

cl_index clasp_print_base(void) {
  T_sp object = cl::_sym_STARprint_baseSTAR->symbolValue();
  cl_index base;
  if (!af_fixnumP(object) || (base = unbox_fixnum(gc::As<Fixnum_sp>(object))) < 2 || base > 36) {
    SIMPLE_ERROR(BF("The value of *PRINT-BASE*\n %s\n"
                    "is not of the expected type (INTEGER 2 36)") %
                 _rep_(object));
  }
  return base;
}

cl_index clasp_print_level(void) {
  T_sp object = cl::_sym_STARprint_levelSTAR->symbolValue();
  gctools::Fixnum level;
  if (object.nilp()) {
    level = MOST_POSITIVE_FIXNUM;
  } else if (af_fixnumP(object)) {
    level = unbox_fixnum(gc::As<Fixnum_sp>(object));
    if (level < 0) {
    ERROR:
      cl::_sym_STARprint_levelSTAR->setf_symbolValue(_Nil<T_O>());
      SIMPLE_ERROR(BF("The value of *PRINT-LEVEL*\n %s\n"
                      "is not of the expected type (or NULL (INTEGER 0 *))") %
                   _rep_(object));
    }
  } else if (af_bignumP(object)) {
    goto ERROR;
  } else {
    level = MOST_POSITIVE_FIXNUM;
  }
  return level;
}

cl_index clasp_print_length(void) {
  T_sp object = cl::_sym_STARprint_lengthSTAR->symbolValue();
  gctools::Fixnum length;
  if (object.nilp()) {
    length = MOST_POSITIVE_FIXNUM;
  } else if (af_fixnumP(object)) {
    length = unbox_fixnum(gc::As<Fixnum_sp>(object));
    if (length < 0) {
    ERROR:
      cl::_sym_STARprint_lengthSTAR->setf_symbolValue(_Nil<T_O>());
      SIMPLE_ERROR(BF("The value of *PRINT-LENGTH*\n %s\n"
                      "is not of the expected type (or NULL (INTEGER 0 *))") %
                   _rep_(object));
    }
  } else if (af_bignumP(object)) {
    goto ERROR;
  } else {
    length = MOST_POSITIVE_FIXNUM;
  }
  return length;
}

bool clasp_print_radix(void) {
  return cl::_sym_STARprint_radixSTAR->symbolValue().isTrue();
}

Symbol_sp clasp_print_case(void) {
  T_sp output = cl::_sym_STARprint_caseSTAR->symbolValue();
  if (Symbol_sp soutput = output.asOrNull<Symbol_O>()) {
    if (soutput == kw::_sym_upcase ||
        soutput == kw::_sym_downcase ||
        soutput == kw::_sym_capitalize) {
      return soutput;
    }
  }
  SIMPLE_ERROR(BF("The value of *PRINT-CASE*\n"
                  "is not of the expected type"
                  "(MEMBER :UPCASE :DOWNCASE :CAPITALIZE)"));
}

bool clasp_print_gensym(void) {
  return cl::_sym_STARprint_gensymSTAR->symbolValue().isTrue();
}

bool clasp_print_array(void) {
  return cl::_sym_STARprint_arraySTAR->symbolValue().isTrue();
}

bool clasp_print_readably(void) {
  return cl::_sym_STARprint_readablySTAR->symbolValue().isTrue();
}

bool clasp_print_escape(void) {
  return cl::_sym_STARprint_escapeSTAR->symbolValue().isTrue();
}

bool clasp_print_circle(void) {
  return cl::_sym_STARprint_circleSTAR->symbolValue().isTrue();
}

#define ARGS_cl_write "(x &key ((:stream strm) nil) (array *print-array*) (base *print-base*) ((:case cas) *print-case*) (circle *print-circle*) (escape *print-escape*) (gensym *print-gensym*) (length *print-length*) (level *print-level*) (lines *print-lines*) (miser_width *print-miser-width*) (pprint_dispatch *print-pprint-dispatch*) (pretty *print-pretty*) (radix *print-radix*) (readably *print-readably*) (right_margin *print-right-margin*))"
#define DECL_cl_write ""
#define DOCS_cl_write "write"
T_sp cl_write(T_sp x, T_sp strm, T_sp array, T_sp base,
              T_sp cas, T_sp circle, T_sp escape, T_sp gensym, T_sp length,
              T_sp level, T_sp lines, T_sp miser_width, T_sp pprint_dispatch,
              T_sp pretty, T_sp radix, T_sp readably, T_sp right_margin) {
  DynamicScopeManager scope(cl::_sym_STARprint_arraySTAR, array);
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_baseSTAR, base);
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_caseSTAR, cas);
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_circleSTAR, circle);
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_escapeSTAR, escape);
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_gensymSTAR, gensym);
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_lengthSTAR, length);
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_levelSTAR, level);
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_linesSTAR, lines);
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_miser_widthSTAR, miser_width);
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_pprint_dispatchSTAR, pprint_dispatch);
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_prettySTAR, pretty);
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_radixSTAR, radix);
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_readablySTAR, readably);
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_right_marginSTAR, right_margin);
  T_sp ostrm = coerce::outputStreamDesignator(strm);
  write_object(x, ostrm);
  clasp_force_output(ostrm);
  return Values(x);
};

#if 0
#define ARGS_af_writeAddr "(arg stream)"
#define DECL_af_writeAddr ""
#define DOCS_af_writeAddr "writeAddr"
    void af_writeAddr(T_sp arg, T_sp stream)
    {_G();
	cl_intptr_t i = arg.intptr();
	for ( int j=sizeof(i)*8-4; j>= 0; j-=4 ) {
	    int k = (i>>j) &0xf;
	    if ( k < 10 )
                clasp_write_char('0'+k,stream);
	    else
                clasp_write_char('a'+k-10,stream);
	}
    }
#endif

#define ARGS_af_printUnreadableObjectFunction "(o stream type id function)"
#define DECL_af_printUnreadableObjectFunction ""
#define DOCS_af_printUnreadableObjectFunction "printUnreadableObjectFunction - see ecl::print_unreadable.d"
void af_printUnreadableObjectFunction(T_sp o, T_sp ostream, T_sp type, T_sp id, T_sp function) {
  _G();
  if (clasp_print_readably()) {
    PRINT_NOT_READABLE_ERROR(o);
  } else if (o.unboundp()) {
    SIMPLE_ERROR(BF("Error! printUnreadableObjectFunction object is Unbound"));
  } else {
    stringstream ss;
    ss << "#<";
    if (type.notnilp()) {
      type = af_type_of(o);
      if (!gc::IsA<Symbol_sp>(type)) {
        type = cl::_sym_StandardObject_O;
      }
      Symbol_sp typesym = gc::As<Symbol_sp>(type);
      ss << typesym->symbolNameAsString();
      ss << " ";
    }
    clasp_write_string(ss.str(), ostream);
    if (function.notnilp()) {
      eval::funcall(function);
    }
    stringstream stail;
    stail << " @";
    stail << o.raw_();
    stail << ">";
    clasp_write_string(stail.str(), ostream);
  }
};

#define ARGS_cl_pprint "(obj &optional stream)"
#define DECL_cl_pprint ""
#define DOCS_cl_pprint "pprint"
void cl_pprint(T_sp obj, T_sp stream) {
  _G();
  DynamicScopeManager scope(cl::_sym_STARprint_escapeSTAR, _lisp->_true());
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_prettySTAR, _lisp->_true());
  stream = coerce::outputStreamDesignator(stream);
  clasp_write_char('\n', stream);
  write_object(obj, stream);
  clasp_force_output(stream);
}

#define ARGS_cl_princ "(obj &optional output-stream-desig)"
#define DECL_cl_princ ""
#define DOCS_cl_princ "See CLHS: princ"
T_sp cl_princ(T_sp obj, T_sp output_stream_desig) {
  _G();
  DynamicScopeManager scope1(cl::_sym_STARprint_escapeSTAR, _Nil<T_O>());
  DynamicScopeManager scope2(cl::_sym_STARprint_readablySTAR, _Nil<T_O>());
  eval::funcall(cl::_sym_write, obj, kw::_sym_stream, output_stream_desig);
  return obj;
}

#define ARGS_cl_prin1 "(obj &optional output-stream-desig)"
#define DECL_cl_prin1 ""
#define DOCS_cl_prin1 "See CLHS: prin1"
T_sp cl_prin1(T_sp obj, T_sp output_stream_desig) {
  _G();
  DynamicScopeManager scope(cl::_sym_STARprint_escapeSTAR, _lisp->_true());
  //  T_sp sout = coerce::outputStreamDesignator(output_stream_desig);
  eval::funcall(cl::_sym_write, obj, kw::_sym_stream, output_stream_desig);
  return obj;
}

#define ARGS_cl_print "(obj &optional output-stream-desig)"
#define DECL_cl_print ""
#define DOCS_cl_print "See CLHS: print"
T_sp cl_print(T_sp obj, T_sp output_stream_desig) {
  _G();
  DynamicScopeManager scope(cl::_sym_STARprint_escapeSTAR, _lisp->_true());
  T_sp sout = coerce::outputStreamDesignator(output_stream_desig);
  clasp_write_string("\n", sout);
  cl_prin1(obj, sout);
  clasp_write_string(" ", sout);
  clasp_force_output(sout);
  return obj;
}

void initialize_print() {
  ClDefun(write);
  //        SYMBOL_EXPORT_SC_(CorePkg,writeAddr);
  //        Defun(writeAddr);
  SYMBOL_EXPORT_SC_(CorePkg, printUnreadableObjectFunction);
  Defun(printUnreadableObjectFunction);
  ClDefun(pprint);
  SYMBOL_EXPORT_SC_(ClPkg, print);
  ClDefun(print);
  SYMBOL_EXPORT_SC_(ClPkg, prin1);
  ClDefun(prin1);
  SYMBOL_EXPORT_SC_(ClPkg, princ);
  ClDefun(princ);
}
};
