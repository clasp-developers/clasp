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
CL_DECLARE((declare (locked)));
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

}; /* core */
