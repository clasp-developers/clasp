#define	DEBUG_LEVEL_FULL

#include "core/common.h"
#include "core/environment.h"
#include "symbolTable.h"
#include "sequence.h"
#include "core/wrappers.h"
namespace core
{

// ----------------------------------------------------------------------
//


    SYMBOL_EXPORT_SC_(KeywordPkg,start);
    SYMBOL_EXPORT_SC_(KeywordPkg,end);
    
    
#define ARGS_af_length "(arg)"
#define DECL_af_length ""
#define DOCS_af_length "See CLHS length - works with SEQUENCES and ACTIVATION_FRAMEs"
    uint af_length(T_sp arg)
    {_G();
	if (arg.nilp()) return 0;
	ASSERTF(arg.pointerp(),BF("The arg is not a pointer"));
	if ( Sequence_sp seq = arg.asOrNull<Sequence_O>() )
	{
	    return seq->length();
	} else if ( ActivationFrame_sp af = arg.asOrNull<ActivationFrame_O>() )
	{
	    return af->length();
	}
	TYPE_ERROR(arg,cl::_sym_Sequence_O);
    };






#define DOCS_af_make_sequence "make_sequence"
#define LOCK_af_make_sequence 0
#define ARGS_af_make_sequence "(type size &key (initial-element nil iesp))"
#define DECL_af_make_sequence ""    
    T_mv af_make_sequence(T_sp type, Fixnum_sp size, T_sp initial_element, T_sp iesp)
    {_G();
	IMPLEMENT_MEF(BF("make-sequence"));
#if 0
	Symbol_sp element_type;
	Fixnum_sp length;
	{MULTIPLE_VALUES_ACCESS(_VALUES)
		_sym_closest_sequence_type->mv_apply(type);
	    element_type = _VALUES->get(0).as<Symbol_O>();
	    length = _VALUES.valueGet(1).as<Fixnum_O>();
	}
	if ( element_type == cl::_sym_list )
	{
	    Cons_sp sequence = eval::funcall(cl::_sym_make_list,size,kw::_sym_initial_element,initial_element).as_or_nil<Cons_O>();
	    return sequence;
	}
	IMPLEMENT_MEF(BF("Implement make_sequence"));
	return(Values(_Nil<T_O>()));
#endif
    };





#define DOCS_af_reverse "reverse"
#define LOCK_af_reverse 1
#define ARGS_af_reverse "(seq)"
#define DECL_af_reverse ""
    T_sp af_reverse(Sequence_sp seq)
    {_G();
	if (seq.nilp())
	{
	    return _Nil<T_O>();
	}
	return seq->reverse();
    };





#define ARGS_af_nreverse "(seq)"
#define DECL_af_nreverse "(declare (locked))"
#define DOCS_af_nreverse "nreverse"
#define LOCK_af_nreverse 1
    T_sp af_nreverse(Sequence_sp seq)
    {_G();
	if (seq.nilp())
	{
	    return _Nil<T_O>();
	}
	return seq->nreverse();
    };




#define ARGS_Sequence_O_setf_elt "((self sequence) index value)"
#define DECL_Sequence_O_setf_elt ""
#define DOCS_Sequence_O_setf_elt "CLHS: setter for elt"
    T_sp Sequence_O::setf_elt(int index, T_sp value)
    {_G();
	SUBCLASS_MUST_IMPLEMENT();
    }



    
    
#define ARGS_af_subseq "(sequence start &optional end)"
#define DECL_af_subseq ""
#define DOCS_af_subseq "subseq"
    T_sp af_subseq(Sequence_sp seq, Fixnum_sp start, T_sp end)
    {_G();
        if ( seq.nilp() ) {
            if ( start->get() == 0 && end.nilp() ) {
                return _Nil<T_O>();
            }
            SIMPLE_ERROR(BF("Illegal arguments for subseq on NIL - they must be (subseq NIL 0 NIL)"));
        }
	return seq->subseq(start->get(),end);
    };



    
    
#define ARGS_af_copy_seq "(seq)"
#define DECL_af_copy_seq ""
#define DOCS_af_copy_seq "copy_seq"
    T_sp af_copy_seq(Sequence_sp seq)
    {_G();
        if ( seq.nilp() ) { return _Nil<T_O>(); };
	return seq->subseq(0,_Nil<T_O>());
    };



    EXPOSE_CLASS(core,Sequence_O);

    void Sequence_O::exposeCando(::core::Lisp_sp lisp)
    {_G();
	::core::class_<Sequence_O>()
//	.initArgs("(self)")
	      .def("elt",&Sequence_O::elt)
	      .def("setf-elt",&Sequence_O::setf_elt,ARGS_Sequence_O_setf_elt,DECL_Sequence_O_setf_elt,DOCS_Sequence_O_setf_elt)
	      .def("elt-set",&Sequence_O::setf_elt,ARGS_Sequence_O_setf_elt,DECL_Sequence_O_setf_elt,DOCS_Sequence_O_setf_elt)
	      .def("setf_subseq",&Sequence_O::setf_subseq)
	      ;
	SYMBOL_EXPORT_SC_(ClPkg,make_sequence);
	Defun(make_sequence);

	SYMBOL_EXPORT_SC_(ClPkg,reverse);
	Defun(reverse);

	SYMBOL_EXPORT_SC_(ClPkg,nreverse);
	Defun(nreverse);

	SYMBOL_EXPORT_SC_(ClPkg,subseq);
	Defun(subseq);

	SYMBOL_EXPORT_SC_(ClPkg,copy_seq);
	Defun(copy_seq);

	SYMBOL_EXPORT_SC_(ClPkg,length);
	af_def(ClPkg,"length",&af_length,ARGS_af_length,DECL_af_length,DOCS_af_length);
    }



    void Sequence_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),Sequence,"","",_LISP)
//	.initArgs("(self)")
	    ;
#endif
    }



#if 0
#if defined(OLD_SERIALIZE)
    void Sequence_O::serialize(::serialize::SNodeP node)
    {
        this->Bases::serialize(node);
	// Archive other instance variables here
    }
#endif
#endif

    void Sequence_O::archiveBase(::core::ArchiveP node)
    {
	// do nothing
    }



/*! From ecl_sequence_start_end */

    size_t_pair sequenceStartEnd(const char* file, uint line, const char* functionName,
				 const string& packageName,
				 Sequence_sp sequence, Fixnum_sp start, Fixnum_sp end)
    {
        size_t_pair p;
	size_t  l;
	p.length = l = af_length(sequence);
	unlikely_if (!af_fixnumP(start) || start->minusp()) {
	    af_wrongTypeKeyArg(file,line,_lisp->internWithPackageName(functionName,packageName.c_str()),
			       kw::_sym_start,start,cl::_sym_UnsignedByte);
        }
        p.start = start->get();
	if ( end.nilp() ) {
	    p.end = l;
	} else {
	    unlikely_if (!af_fixnumP(end) || end->minusp()) {
		af_wrongTypeKeyArg(file,line,_lisp->internWithPackageName(functionName,packageName.c_str()),
				   kw::_sym_end,end,
				   Cons_O::createList(cl::_sym_or,cl::_sym_Null_O,cl::_sym_UnsignedByte));
	    }
	    p.end = end->get();
	    unlikely_if (p.end > l) {
		T_sp fillp = Fixnum_O::create(static_cast<uint>(l));
		af_wrongTypeKeyArg(file,line,_lisp->internWithPackageName(functionName,packageName.c_str()),
				   kw::_sym_end, end,
				   Integer_O::makeIntegerType(start->get(),static_cast<int>(l)));
	    }
	}
        unlikely_if (p.end < p.start) {
	    af_wrongTypeKeyArg(file,line,_lisp->internWithPackageName(functionName,packageName.c_str()),
			       kw::_sym_start,start,
			       Integer_O::makeIntegerType(0,static_cast<uint>(p.end)));
        }
        return p;
    }
    

}; /* core */
