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
//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/array.h>
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
  if (!core__fixnump(object) || (base = unbox_fixnum(gc::As<Fixnum_sp>(object))) < 2 || base > 36) {
    SIMPLE_ERROR(BF("The value of *PRINT-BASE*\n %s\n"
                    "is not of the expected type (INTEGER 2 36)") %
                 _rep_(object));
  }
  return base;
}

// see comment for clasp_print_length
cl_index clasp_print_level(void) {
  T_sp object = cl::_sym_STARprint_levelSTAR->symbolValue();
  gctools::Fixnum level;
  // See note about fixnumness in clasp_print_length.
  if (object.nilp()) {
    level = MOST_POSITIVE_FIXNUM;
  } else if (core__fixnump(object)) {
    level = unbox_fixnum(gc::As<Fixnum_sp>(object));
    if (level < 0) {
    ERROR:
      {
        // Bind *print-level* to something valid so that we don't get
        // recursive errors while printing error messages.
        DynamicScopeManager scope(cl::_sym_STARprint_levelSTAR, _Nil<T_O>());
        SIMPLE_ERROR(BF("The value of *PRINT-LEVEL*\n %s\n"
                        "is not of the expected type (OR NULL (INTEGER 0))")
                     % _rep_(object));
      }
    }
  } else if (core__bignump(object)) {
    // FIXME?: We could check if it's negative here to be really scrupulous.
    level = MOST_POSITIVE_FIXNUM;
  } else {
    goto ERROR;
  }
  return level;
}

// Not correct wrt ansi, theoretically could be a bignum
// is only used twice, perhpas we could also allow a bignum
// The practical use of a bignum *print_length* remains to be seen
cl_index clasp_print_length(void) {
  T_sp object = cl::_sym_STARprint_lengthSTAR->symbolValue();
  gctools::Fixnum length;
  // We pretty much assume we'll never have a sequence with a bignum count of elements.
  // This is standardly true with vectors, but lists could hypothetically have any
  // number of elements. Hypothetically. Still, keep this in mind.
  if (object.nilp()) {
    length = MOST_POSITIVE_FIXNUM;
  } else if (core__fixnump(object)) {
    length = unbox_fixnum(gc::As<Fixnum_sp>(object));
    if (length < 0) {
    ERROR:
      {
        DynamicScopeManager scope(cl::_sym_STARprint_lengthSTAR, _Nil<T_O>());
        SIMPLE_ERROR(BF("The value of *PRINT-LENGTH*\n %s\n"
                        "is not of the expected type (OR NULL (INTEGER 0))")
                     % _rep_(object));
      }
    }
  } else if (core__bignump(object)) {
    length = MOST_POSITIVE_FIXNUM;
  } else {
    goto ERROR;
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
  TYPE_ERROR(output,Cons_O::createList(cl::_sym_member,kw::_sym_upcase,kw::_sym_downcase,kw::_sym_capitalize));
}

bool clasp_print_gensym(void) {
  return cl::_sym_STARprint_gensymSTAR->symbolValue().isTrue();
}

bool clasp_print_array(void) {
  return cl::_sym_STARprint_arraySTAR->symbolValue().isTrue();
}

bool clasp_print_readably(void) {
  unlikely_if (!cl::_sym_STARprint_readablySTAR) return false;
  return cl::_sym_STARprint_readablySTAR->symbolValue().isTrue();
}

bool clasp_print_escape(void) {
  unlikely_if (!cl::_sym_STARprint_escapeSTAR) return false;
  return cl::_sym_STARprint_escapeSTAR->symbolValue().isTrue();
}

bool clasp_print_circle(void) {
  return cl::_sym_STARprint_circleSTAR->symbolValue().isTrue();
}

CL_LAMBDA(x &key ((:stream strm) nil) (array *print-array*) (base *print-base*) ((:case cas) *print-case*) (circle *print-circle*) (escape *print-escape*) (gensym *print-gensym*) (length *print-length*) (level *print-level*) (lines *print-lines*) (miser-width *print-miser-width*) (pprint-dispatch *print-pprint-dispatch*) (pretty *print-pretty*) (radix *print-radix*) (readably *print-readably*) (right-margin *print-right-margin*));
CL_DECLARE();
CL_DOCSTRING("write");
CL_DEFUN T_sp cl__write(T_sp x, T_sp strm, T_sp array, T_sp base,
              T_sp cas, T_sp circle, T_sp escape, T_sp gensym, T_sp length,
              T_sp level, T_sp lines, T_sp miser_width, T_sp pprint_dispatch,
              T_sp pretty, T_sp radix, T_sp readably, T_sp right_margin) {
  DynamicScopeManager scope(cl::_sym_STARprint_arraySTAR, array);
  DynamicScopeManager scope1(cl::_sym_STARprint_baseSTAR, base);
  DynamicScopeManager scope2(cl::_sym_STARprint_caseSTAR, cas);
  DynamicScopeManager scope3(cl::_sym_STARprint_circleSTAR, circle);
  DynamicScopeManager scope4(cl::_sym_STARprint_escapeSTAR, escape);
  DynamicScopeManager scope5(cl::_sym_STARprint_gensymSTAR, gensym);
  DynamicScopeManager scope6(cl::_sym_STARprint_lengthSTAR, length);
  DynamicScopeManager scope7(cl::_sym_STARprint_levelSTAR, level);
  DynamicScopeManager scope8(cl::_sym_STARprint_linesSTAR, lines);
  DynamicScopeManager scope9(cl::_sym_STARprint_miser_widthSTAR, miser_width);
  DynamicScopeManager scopeA(cl::_sym_STARprint_pprint_dispatchSTAR, pprint_dispatch);
  DynamicScopeManager scopeB(cl::_sym_STARprint_prettySTAR, pretty);
  DynamicScopeManager scopeC(cl::_sym_STARprint_radixSTAR, radix);
  DynamicScopeManager scopeD(cl::_sym_STARprint_readablySTAR, readably);
  DynamicScopeManager scopeE(cl::_sym_STARprint_right_marginSTAR, right_margin);
  T_sp ostrm = coerce::outputStreamDesignator(strm);
  write_object(x, ostrm);
  return Values(x);
};


CL_LAMBDA(o stream type id function);
CL_DECLARE();
CL_DOCSTRING("print-unreadable-object-function: What CL:PRINT-UNREADABLE-OBJECT expands into.");
CL_DEFUN T_sp core__print_unreadable_object_function(T_sp object, T_sp output_stream_desig, T_sp type, T_sp id, T_sp function) {
  if (clasp_print_readably()) {
    PRINT_NOT_READABLE_ERROR(object);
  } else if (object.unboundp()) {
    SIMPLE_ERROR(BF("Error! printUnreadableObjectFunction object is Unbound"));
  } else {
    stringstream ss;
    ss << "#<";
    if (type.notnilp()) {
      type = cl__type_of(object);
      if (!gc::IsA<Symbol_sp>(type)) {
        type = cl::_sym_standard_object;
      }
      Symbol_sp typesym = gc::As<Symbol_sp>(type);
      ss << typesym->symbolNameAsString();
      ss << " ";
    }
    T_sp ostream = coerce::outputStreamDesignator(output_stream_desig);
    clasp_write_string(ss.str(), ostream);
    if (function.notnilp()) {
      eval::funcall(function);
    }
    stringstream stail;
    if (id.notnilp()) {
      stail << " @";
      stail << object.raw_();
    }
    stail << ">";
    clasp_write_string(stail.str(), ostream);
  }
  return _Nil<T_O>();
};

CL_LAMBDA(obj &optional stream);
CL_DECLARE();
CL_DOCSTRING("pprint");
CL_DEFUN void cl__pprint(T_sp obj, T_sp stream) {
  DynamicScopeManager scope(cl::_sym_STARprint_escapeSTAR, _lisp->_true());
  DynamicScopeManager scope1(cl::_sym_STARprint_prettySTAR, _lisp->_true());
  stream = coerce::outputStreamDesignator(stream);
  clasp_write_char('\n', stream);
  write_object(obj, stream);
}

CL_LAMBDA(obj &optional output-stream-desig);
CL_DECLARE();
CL_DOCSTRING("See CLHS: princ");
CL_DEFUN T_sp cl__princ(T_sp obj, T_sp output_stream_desig) {
  DynamicScopeManager scope1(cl::_sym_STARprint_escapeSTAR, _Nil<T_O>());
  DynamicScopeManager scope2(cl::_sym_STARprint_readablySTAR, _Nil<T_O>());
  eval::funcall(cl::_sym_write, obj, kw::_sym_stream, output_stream_desig);
  return obj;
}

CL_LAMBDA(obj &optional output-stream-desig);
CL_DECLARE();
CL_DOCSTRING("See CLHS: prin1");
CL_DEFUN T_sp cl__prin1(T_sp obj, T_sp output_stream_desig) {
  DynamicScopeManager scope(cl::_sym_STARprint_escapeSTAR, _lisp->_true());
  //  T_sp sout = coerce::outputStreamDesignator(output_stream_desig);
//  printf("%s:%d cl__prin1  kw::_sym_stream@%p\n", __FILE__, __LINE__, kw::_sym_stream.raw_());
  eval::funcall(cl::_sym_write, obj, kw::_sym_stream, output_stream_desig);
  return obj;
}

CL_LAMBDA(obj &optional output-stream-desig);
CL_DECLARE();
CL_DOCSTRING("See CLHS: print");
CL_DEFUN T_sp cl__print(T_sp obj, T_sp output_stream_desig) {
  DynamicScopeManager scope(cl::_sym_STARprint_escapeSTAR, _lisp->_true());
  T_sp sout = coerce::outputStreamDesignator(output_stream_desig);
  clasp_write_string("\n", sout);
  cl__prin1(obj, sout);
  clasp_write_string(" ", sout);
  return obj;
}

  SYMBOL_EXPORT_SC_(CorePkg, printUnreadableObjectFunction);
  SYMBOL_EXPORT_SC_(ClPkg, print);
  SYMBOL_EXPORT_SC_(ClPkg, prin1);
  SYMBOL_EXPORT_SC_(ClPkg, princ);

};
