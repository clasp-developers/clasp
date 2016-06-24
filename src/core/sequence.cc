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
#define DEBUG_LEVEL_FULL

#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispVector.h>
#include <clasp/core/sequence.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

SYMBOL_EXPORT_SC_(KeywordPkg, start);
SYMBOL_EXPORT_SC_(KeywordPkg, end);

LAMBDA(arg);
DECLARE();
DOCSTRING("See CLHS length - works with SEQUENCES and ACTIVATION_FRAMEs");
CL_DEFUN uint cl__length(T_sp arg) {
  if (arg.nilp()) {
    return 0;
  } else if (Cons_sp l = arg.asOrNull<Cons_O>()) {
    return l->length();
  } else if (Vector_sp vec = arg.asOrNull<Vector_O>()) {
    return vec->length();
  } else if (ActivationFrame_sp af = arg.asOrNull<ActivationFrame_O>()) {
    return af->length();
  }
  TYPE_ERROR(arg, cl::_sym_sequence);
};

LAMBDA(sequence index);
DECLARE();
DOCSTRING("elt");
CL_DEFUN T_sp cl__elt(T_sp sequence, int index) {
  _G();
  if (sequence.nilp()) {
    TYPE_ERROR(sequence, cl::_sym_sequence);
  } else if (Cons_sp cseq = sequence.asOrNull<Cons_O>()) {
    return cseq->elt(index);
  } else if (Vector_sp vseq = sequence.asOrNull<Vector_O>()) {
    return vseq->elt(index);
  }
  TYPE_ERROR(sequence, cl::_sym_sequence);
};

LAMBDA(type size &key (initial-element nil iesp));
DECLARE();
DOCSTRING("make_sequence");
CL_DEFUN T_mv cl__make_sequence(T_sp type, Fixnum_sp size, T_sp initial_element, T_sp iesp) {
  _G();
  IMPLEMENT_MEF(BF("make-sequence"));
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
	IMPLEMENT_MEF(BF("Implement make_sequence"));
	return(Values(_Nil<T_O>()));
#endif
};

LAMBDA(seq);
DECLARE();
DOCSTRING("reverse");
CL_DEFUN T_sp cl__reverse(T_sp seq) {
  _G();
  if (seq.nilp()) {
    return _Nil<T_O>();
  } else if (Vector_sp svec = seq.asOrNull<Vector_O>()) {
    return svec->reverse();
  } else if (Cons_sp slist = seq.asOrNull<Cons_O>()) {
    return slist->reverse();
  }
  TYPE_ERROR(seq, cl::_sym_sequence);
};

LAMBDA(seq);
DECLARE((declare (locked)));
DOCSTRING("nreverse");
CL_DEFUN T_sp cl__nreverse(T_sp seq) {
  _G();
  if (seq.nilp()) {
    return _Nil<T_O>();
  } else if (Vector_sp svec = seq.asOrNull<Vector_O>()) {
    return svec->nreverse();
  } else if (Cons_sp slist = seq.asOrNull<Cons_O>()) {
    return slist->nreverse();
  }
  TYPE_ERROR(seq, cl::_sym_sequence);
};

LAMBDA(sequence start &optional end);
DECLARE();
DOCSTRING("subseq");
CL_DEFUN T_sp cl__subseq(T_sp seq, int start, T_sp end) {
  _G();
  if (seq.nilp()) {
    if (start == 0 && (end.nilp() || (end.fixnump() && unbox_fixnum(gc::As<Fixnum_sp>(end)) == 0))) {
      return _Nil<T_O>();
    }
    SIMPLE_ERROR(BF("Illegal arguments for subseq on NIL - they must be (subseq NIL 0 NIL)"));
  } else if (Vector_sp vseq = seq.asOrNull<Vector_O>()) {
    return vseq->subseq(start, end);
  } else if (Cons_sp cseq = seq.asOrNull<Cons_O>()) {
    return cseq->subseq(start, end);
  }
  TYPE_ERROR(seq, cl::_sym_sequence);
};

LAMBDA(seq);
DECLARE();
DOCSTRING("copy_seq");
CL_DEFUN T_sp cl__copy_seq(T_sp seq) {
  _G();
  if (seq.nilp()) {
    return _Nil<T_O>();
  } else if (Cons_sp cseq = seq.asOrNull<Cons_O>()) {
    return cseq->subseq(0, _Nil<T_O>());
  } else if (Vector_sp vseq = seq.asOrNull<Vector_O>()) {
    return vseq->subseq(0, _Nil<T_O>());
  }
  TYPE_ERROR(seq, cl::_sym_sequence);
};

LAMBDA(sequence index value);
DECLARE();
DOCSTRING("setfElt");
CL_DEFUN T_sp core__setf_elt(T_sp sequence, int index, T_sp value) {
  _G();
  if (sequence.nilp()) {
    TYPE_ERROR(sequence, cl::_sym_sequence);
  } else if (Vector_sp vsequence = sequence.asOrNull<Vector_O>()) {
    vsequence->setf_elt(index, value);
    return value;
  } else if (Cons_sp vcons = sequence.asOrNull<Cons_O>()) {
    vcons->setf_elt(index, value);
    return value;
  }
  TYPE_ERROR(sequence, cl::_sym_sequence);
};

LAMBDA(seq index val);
DECLARE();
DOCSTRING("eltSet");
CL_DEFUN T_sp core__elt_set(T_sp sequence, int index, T_sp val) {
  _G();
  return core__setf_elt(sequence, index, val);
};

LAMBDA(sequence start end subseq);
DECLARE();
DOCSTRING("setfSubseq");
CL_DEFUN T_sp core__setf_subseq(T_sp sequence, int start, Fixnum_sp end, T_sp subseq) {
  _G();
  if (sequence.nilp()) {
    TYPE_ERROR(sequence, cl::_sym_sequence);
  } else if (Vector_sp vsequence = sequence.asOrNull<Vector_O>()) {
    vsequence->setf_subseq(start, end, subseq);
    return subseq;
  } else if (Cons_sp vcons = sequence.asOrNull<Cons_O>()) {
    vcons->setf_subseq(start, end, subseq);
    return subseq;
  }
  TYPE_ERROR(sequence, cl::_sym_sequence);
};

#if 0
    EXPOSE_CLASS(core,Sequence_O);

    void Sequence_O::exposeCando(::core::Lisp_sp lisp)
    {_G();
	::core::class_<Sequence_O>()
//	.initArgs("(self)")
	      .def("elt",&Sequence_O::elt)
	      .def("core:setf-elt",&Sequence_O::setf_elt,ARGS_Sequence_O_setf_elt,DECL_Sequence_O_setf_elt,DOCS_Sequence_O_setf_elt)
	      .def("core:elt-set",&Sequence_O::setf_elt,ARGS_Sequence_O_setf_elt,DECL_Sequence_O_setf_elt,DOCS_Sequence_O_setf_elt)
	      .def("core:setf_subseq",&Sequence_O::setf_subseq)
	      ;
    };
#endif

void initialize_sequence() {
  SYMBOL_EXPORT_SC_(CorePkg, setfElt);

  SYMBOL_EXPORT_SC_(CorePkg, eltSet);


  SYMBOL_EXPORT_SC_(CorePkg, setfSubseq);

  SYMBOL_EXPORT_SC_(ClPkg, make_sequence);

  SYMBOL_EXPORT_SC_(ClPkg, reverse);

  SYMBOL_EXPORT_SC_(ClPkg, nreverse);

  SYMBOL_EXPORT_SC_(ClPkg, subseq);

  SYMBOL_EXPORT_SC_(ClPkg, copySeq);

  SYMBOL_EXPORT_SC_(ClPkg, length);
}

#if 0
    void Sequence_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),Sequence,"","",_LISP)
//	.initArgs("(self)")
	    ;
#endif
    }
#endif

#if 0
#if defined(OLD_SERIALIZE)
    void Sequence_O::serialize(::serialize::SNodeP node)
    {
        this->Bases::serialize(node);
	// Archive other instance variables here
    }
#endif
#endif

#if 0
    void Sequence_O::archiveBase(::core::ArchiveP node)
    {
	// do nothing
    }
#endif

/*! From ecl_sequence_start_end */

size_t_pair sequenceStartEnd(const char *file, uint line, const char *functionName,
                             const string &packageName,
                             T_sp sequence, Fixnum_sp start, T_sp end) {
  size_t_pair p;
  size_t l;
  p.length = l = cl__length(sequence);
  unlikely_if(!af_fixnumP(start) || clasp_minusp(start)) {
    af_wrongTypeKeyArg(file, line, _lisp->internWithPackageName(functionName, packageName.c_str()),
                       kw::_sym_start, start, cl::_sym_UnsignedByte);
  }
  p.start = unbox_fixnum(start);
  if (end.nilp()) {
    p.end = l;
  } else {
    unlikely_if(!af_fixnumP(end) || clasp_minusp(gc::As<Fixnum_sp>(end))) {
      af_wrongTypeKeyArg(file, line, _lisp->internWithPackageName(functionName, packageName.c_str()),
                         kw::_sym_end, end,
                         Cons_O::createList(cl::_sym_or, cl::_sym_null, cl::_sym_UnsignedByte));
    }
    p.end = unbox_fixnum(gc::As<Fixnum_sp>(end));
    unlikely_if(p.end > l) {
      //      T_sp fillp = make_fixnum(static_cast<uint>(l));
      af_wrongTypeKeyArg(file, line, _lisp->internWithPackageName(functionName, packageName.c_str()),
                         kw::_sym_end, end,
                         Integer_O::makeIntegerType(unbox_fixnum(start), static_cast<int>(l)));
    }
  }
  unlikely_if(p.end < p.start) {
    af_wrongTypeKeyArg(file, line, _lisp->internWithPackageName(functionName, packageName.c_str()),
                       kw::_sym_start, start,
                       Integer_O::makeIntegerType(0, static_cast<uint>(p.end)));
  }
  return p;
}

}; /* core */
