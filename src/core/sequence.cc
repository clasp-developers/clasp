/*
    File: sequence.cc
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
#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/array.h>
#include <clasp/core/sequence.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

SYMBOL_EXPORT_SC_(KeywordPkg, start);
SYMBOL_EXPORT_SC_(KeywordPkg, end);

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("See CLHS length - works with SEQUENCES and ACTIVATION_FRAMEs");
CL_DEFUN size_t cl__length(T_sp arg) {
  if (arg.consp()) {
    return arg.unsafe_cons()->length();
  } else if (arg.nilp()) {
    return 0;
  } else if (Vector_sp vec = arg.asOrNull<Vector_O>()) {
    return vec->length();
  } else if (ActivationFrame_sp af = arg.asOrNull<ActivationFrame_O>()) {
    return af->length();
  }
  TYPE_ERROR(arg, cl::_sym_sequence);
};

#if 0
CL_LAMBDA(type size &key (initial-element nil iesp));
CL_DECLARE();
CL_DOCSTRING("make_sequence");
CL_DEFUN T_mv cl__make_sequence(T_sp type, Fixnum_sp size, T_sp initial_element, T_sp iesp) {
  IMPLEMENT_MEF("make-sequence");
#if 0
	Symbol_sp element_type;
	Fixnum_sp length;
	{MULTIPLE_VALUES_ACCESS(_VALUES)
		_sym_closest_sequence_type->mv_apply(type);
	    element_type = _VALUES->get(0).as<Symbol_O>();
	    length = gc::As<Fixnum_sp>(_VALUES.valueGet(1));
	}
	if ( element_type == cl::_sym_list )
	{
	    List_sp sequence = eval::funcall(cl::_sym_make_list,size,kw::_sym_initial_element,initial_element);
	    return sequence;
	}
	IMPLEMENT_MEF("Implement make_sequence");
	return(Values(_Nil<T_O>()));
#endif
};
#endif

CL_LAMBDA(seq);
CL_DECLARE();
CL_DOCSTRING("reverse");
CL_DEFUN T_sp cl__reverse(T_sp seq) {
  if (seq.consp()) {
    return seq.unsafe_cons()->reverse();
  } else if (seq.nilp()) {
    return _Nil<T_O>();
  } else if (Array_sp arr = seq.asOrNull<Array_O>()) {
    return arr->reverse();
  }
  TYPE_ERROR(seq, cl::_sym_sequence);
};

CL_LAMBDA(seq);
CL_DECLARE();
CL_DOCSTRING("nreverse");
CL_DEFUN T_sp cl__nreverse(T_sp seq) {
  if (seq.consp()) {
    return seq.unsafe_cons()->nreverse();
  } else if (seq.nilp()) {
    return _Nil<T_O>();
  } else if (Array_sp arr = seq.asOrNull<Array_O>()) {
    return arr->nreverse();
  }
  TYPE_ERROR(seq, cl::_sym_sequence);
};

CL_LAMBDA(sequence start &optional end);
CL_DECLARE();
CL_DOCSTRING("subseq");
CL_DEFUN T_sp cl__subseq(T_sp seq, size_t start, T_sp end) {
  if (seq.consp()) {
    return seq.unsafe_cons()->subseq(start, end);
  } else if (seq.nilp()) {
    if (start == 0 && (end.nilp() || (end.fixnump() && unbox_fixnum(gc::As<Fixnum_sp>(end)) == 0))) {
      return _Nil<T_O>();
    }
    SIMPLE_ERROR(BF("Illegal arguments for subseq on NIL - they must be (subseq NIL 0 NIL)"));
  } else if (Vector_sp vseq = seq.asOrNull<Vector_O>()) {
    return vseq->subseq(start, end);
  }
  TYPE_ERROR(seq, cl::_sym_sequence);
};

CL_LAMBDA(seq);
CL_DECLARE();
CL_DOCSTRING("copy_seq");
CL_DEFUN T_sp cl__copy_seq(T_sp seq) {
  if (seq.consp()) {
    return seq.unsafe_cons()->subseq(0, _Nil<T_O>());
  } else if (seq.nilp()) {
    return _Nil<T_O>();
  } else if (Vector_sp vseq = seq.asOrNull<Vector_O>()) {
    return vseq->subseq(0, _Nil<T_O>());
  }
  TYPE_ERROR(seq, cl::_sym_sequence);
};

CL_LAMBDA(sequence index);
CL_DECLARE();
CL_DOCSTRING("elt");
CL_DEFUN T_sp cl__elt(T_sp sequence, size_t index) {
  if (sequence.consp()) {
    return sequence.unsafe_cons()->elt(index);
  } else if (sequence.nilp()) {
    TYPE_ERROR(sequence, cl::_sym_sequence);
  } else if (Vector_sp vseq = sequence.asOrNull<Vector_O>()) {
    return vseq->rowMajorAref(index);
  }
  TYPE_ERROR(sequence, cl::_sym_sequence);
};


CL_LAMBDA(sequence index value);
CL_DECLARE();
CL_DOCSTRING("setfElt");
CL_DEFUN T_sp core__setf_elt(T_sp sequence, size_t index, T_sp value) {
  if (sequence.consp()) {
    sequence.unsafe_cons()->setf_elt(index, value);
    return value;
  } else if (sequence.nilp()) {
    TYPE_ERROR(sequence, cl::_sym_sequence);
  } else if (Vector_sp vsequence = sequence.asOrNull<Vector_O>()) {
    vsequence->rowMajorAset(index, value);
    return value;
  }
  TYPE_ERROR(sequence, cl::_sym_sequence);
};

CL_LAMBDA(sequence start end subseq);
CL_DECLARE();
CL_DOCSTRING("setfSubseq");
CL_DEFUN T_sp core__setf_subseq(T_sp sequence, size_t start, Fixnum_sp end, T_sp subseq) {
  if (sequence.consp()) {
    sequence.unsafe_cons()->setf_subseq(start, end, subseq);
    return subseq;
  } else if (sequence.nilp()) {
    TYPE_ERROR(sequence, cl::_sym_sequence);
  } else if (Vector_sp vsequence = sequence.asOrNull<Vector_O>()) {
    vsequence->setf_subseq(start, end, subseq);
    return subseq;
  }
  TYPE_ERROR(sequence, cl::_sym_sequence);
};

  SYMBOL_EXPORT_SC_(CorePkg, setfElt);
  SYMBOL_EXPORT_SC_(CorePkg, eltSet);
  SYMBOL_EXPORT_SC_(CorePkg, setfSubseq);
  SYMBOL_EXPORT_SC_(ClPkg, make_sequence);
  SYMBOL_EXPORT_SC_(ClPkg, reverse);
  SYMBOL_EXPORT_SC_(ClPkg, nreverse);
  SYMBOL_EXPORT_SC_(ClPkg, subseq);
  SYMBOL_EXPORT_SC_(ClPkg, copySeq);
  SYMBOL_EXPORT_SC_(ClPkg, length);


/*! From ecl_sequence_start_end */

size_t_pair sequenceKeywordStartEnd(Symbol_sp fn_name, T_sp sequence, Fixnum_sp start, T_sp end) {
  size_t_pair p;
  size_t l;
  p.length = l = cl__length(sequence);
  unlikely_if(!core__fixnump(start) || clasp_minusp(start)) {
    ERROR_WRONG_TYPE_KEY_ARG(fn_name,
                             kw::_sym_start, start, cl::_sym_UnsignedByte);
  }
  p.start = unbox_fixnum(start);
  if (end.nilp()) {
    p.end = l;
  } else {
    unlikely_if(!core__fixnump(end) || clasp_minusp(gc::As<Fixnum_sp>(end))) {
      ERROR_WRONG_TYPE_KEY_ARG(fn_name,
                               kw::_sym_end, end,
                               Cons_O::createList(cl::_sym_or, cl::_sym_null, cl::_sym_UnsignedByte));
    }
    p.end = unbox_fixnum(gc::As<Fixnum_sp>(end));
    unlikely_if(p.end > l) {
      //      T_sp fillp = make_fixnum(static_cast<uint>(l));
      ERROR_WRONG_TYPE_KEY_ARG( fn_name,
                                kw::_sym_end, end,
                                Integer_O::makeIntegerType(unbox_fixnum(start), static_cast<int>(l)));
    }
  }
  unlikely_if(p.end < p.start) {
    ERROR_WRONG_TYPE_KEY_ARG(fn_name,
                             kw::_sym_start, start,
                             Integer_O::makeIntegerType(0, static_cast<uint>(p.end)));
  }
  return p;
}

void sequenceIndexInBounds(Symbol_sp fn_name, size_t vector_length, size_t index) {
  unlikely_if(index<0) {
    FUNCTION_WRONG_TYPE_ARG(fn_name, clasp_make_fixnum(index), cl::_sym_UnsignedByte);
  }
  unlikely_if(vector_length < index) {
    FUNCTION_WRONG_TYPE_ARG(fn_name, clasp_make_fixnum(index),
                            Integer_O::makeIntegerType(0, static_cast<uint>(vector_length-1)));
  }
}


size_t_pair sequenceStartEnd(Symbol_sp fn_name, size_t vector_length, size_t start, T_sp end) {
  size_t_pair p;
  size_t l;
  p.length = l = vector_length;
  unlikely_if(start<0) {
    FUNCTION_WRONG_TYPE_ARG(fn_name, clasp_make_fixnum(start), cl::_sym_UnsignedByte);
  }
  p.start = start;
  if (end.nilp()) {
    p.end = l;
  } else {
    unlikely_if(!core__fixnump(end) || clasp_minusp(gc::As<Fixnum_sp>(end))) {
      FUNCTION_WRONG_TYPE_ARG(fn_name, end,
                              Cons_O::createList(cl::_sym_or, cl::_sym_null, cl::_sym_UnsignedByte));
    }
    p.end = unbox_fixnum(gc::As<Fixnum_sp>(end));
    unlikely_if(p.end > l) {
      //      T_sp fillp = make_fixnum(static_cast<uint>(l));
      FUNCTION_WRONG_TYPE_ARG( fn_name, end,
                               Integer_O::makeIntegerType(start, static_cast<int>(l)));
    }
  }
  unlikely_if(p.end < p.start) {
    FUNCTION_WRONG_TYPE_ARG(fn_name, clasp_make_fixnum(start),
                            Integer_O::makeIntegerType(0, static_cast<uint>(p.end)));
  }
  return p;
}

#if 0
void inBoundsBelowEndOrError(cl_index index, cl_index start, cl_index end ) {
  unlikely_if ( index < start ) {
    SIMPLE_ERROR(BF("The index %d must be greator or equal to %d and less than %d") % index % start % end );
  }
  unlikely_if ( index >= end ) {
    SIMPLE_ERROR(BF("Out of bounds index %d must be a value from %d and less than %d") % index % start % end );
  }
  return;
}

void inBoundsOrError(Symbol_sp function_name, cl_index index, cl_index start, cl_index end ) {
  unlikely_if ( index < start ) {
    SIMPLE_ERROR(BF("The index %d must be greator or equal to %d and less than %d") % index % start % end );
  }
  unlikely_if ( index > end ) {
    SIMPLE_ERROR(BF("Out of bounds index %d must be less than or equal to %d") % index % end );
  }
  return;
}


/*! if end is a fixnum then check that its in [start,iend) and if
    not signal an error.   Otherwise return it.
    if end is NIL then return iend, otherwise error.
*/
cl_index coerceToEndInRangeOrError(T_sp end, cl_index start, cl_index iend )
{
  if (end.fixnump()) {
    cl_index rend = end.unsafe_fixnum();
    inBoundsOrError(rend,start,iend);
    return rend;
  }
  if (end.nilp()) return iend;
  SIMPLE_ERROR(BF("%s is an illegal designator for the end") % _rep_(end));
}

#endif
}; /* core */
