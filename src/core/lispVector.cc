/*
    File: lispVector.cc
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
#define	DEBUG_LEVEL_FULL

#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispVector.h>
#include <clasp/core/str.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/strWithFillPtr.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
#include <clasp/core/character.h>
#include <clasp/core/wrappers.h>
namespace core
{

// ----------------------------------------------------------------------
//


    
    
#define ARGS_cl_vector "(&rest args)"
#define DECL_cl_vector ""
#define DOCS_cl_vector "vector"
    Vector_sp cl_vector(List_sp args)
    {_G();
	Vector_sp vec = VectorObjects_O::make(_Nil<T_O>(),args,cl_length(args),false);
        return vec;
    };



#define DOCS_af_make_vector "make_vector See si_make_vector in ecl>>array.d"
#define ARGS_af_make_vector "(element-type dimension adjustable fill-pointer displaced-to displaced-index-offset &optional initial-element initial-contents)"
#define DECL_af_make_vector ""    
    SYMBOL_EXPORT_SC_(ClPkg,subtypep);
    Vector_sp af_make_vector(T_sp element_type,
			int dimension,
			bool adjustable,
			T_sp fill_pointer,
			T_sp displaced_to,
			T_sp displaced_index_offset,
			T_sp initial_element,
			T_sp initialContents )
    {_G();
	ASSERTF(displaced_to.nilp(),BF("Add support for make-vector :displaced-to"));
	ASSERTF(displaced_index_offset.nilp() || displaced_index_offset.as<Fixnum_O>()->get()==0,BF("Add support for make-vector non-zero :displaced-index-offset "));
	if ( element_type == cl::_sym_bit )
	{
	    IMPLEMENT_MEF(BF("Handle bitvectors"));
	} else if ( element_type == cl::_sym_BaseChar_O
		    || element_type == cl::_sym_Character_O
		    || element_type == cl::_sym_StandardChar_O
		    || element_type == cl::_sym_ExtendedChar_O )
	{
	    // Currently any kind of Character vector is a Str or subclass
	    // TODO: Maybe use other types of strings - unicode?
	    char c = ' ';
	    if ( Character_sp cc = initial_element.asOrNull<Character_O>() )
	    {
		c = cc->asChar();
	    }
	    if ( fill_pointer.notnilp() )
	    {
		int ifp = 0;
		if ( fill_pointer == _lisp->_true() ) ifp = dimension;
		else ifp = MIN(dimension,abs(unbox_fixnum(fill_pointer.as<Fixnum_O>())));
		return StrWithFillPtr_O::create(c,dimension,ifp,adjustable,initialContents);
	    }
	    return(Str_O::create(' ',dimension,initialContents));
	} else {
	    if ( cl_consp(element_type)) {
		// For type = '(unsigned-byte XXX) set initial_element if it hasn't been set
		Cons_sp cet = element_type.as<Cons_O>(); 
		if ( oCar(cet) == cl::_sym_UnsignedByte
		     && initial_element.nilp() ) {
		    initial_element = make_fixnum(0);
		}
	    }
	    if ( fill_pointer.notnilp() )
	    {
		int ifp = 0;
		if ( fill_pointer == _lisp->_true() ) ifp = dimension;
		else ifp = unbox_fixnum(fill_pointer.as<Fixnum_O>());
		return VectorObjectsWithFillPtr_O::make(initial_element,initialContents,dimension,ifp,adjustable);
	    } else
	    {
		return VectorObjects_O::make(initial_element,initialContents,dimension,adjustable);
	    }
	}
	SIMPLE_ERROR(BF("Handle make-vector :element-type %s") % _rep_(element_type) );
    };



    
    
#define ARGS_core_adjustVector "(array dimensions initial-element initial-contents)"
#define DECL_core_adjustVector ""
#define DOCS_core_adjustVector "adjustVector"
    T_sp core_adjustVector(T_sp array, int new_dimensions, T_sp initial_element, List_sp initial_contents )
    {_G();
	if ( VectorObjects_sp vo = array.asOrNull<VectorObjects_O>() )
	{
	    vo->adjust(initial_element, initial_contents, new_dimensions );
	    return vo;
	}
	IMPLEMENT_MEF(BF("Implement adjustVector for: %s")% _rep_(array) );
    };







    void Vector_O::initialize()
    {_OF();
        this->Base::initialize();
    }


    void Vector_O::archiveBase(::core::ArchiveP node)
    {
	// Do nothing
    }


    int Vector_O::arrayDimension(int axisNumber) const
    {
	ASSERTF(axisNumber==0,BF("Illegal axis number %d for Vector") % axisNumber);
	return this->dimension();
    }

    List_sp Vector_O::arrayDimensions() const
    {
	return Cons_O::create(make_fixnum(this->dimension()),_Nil<T_O>());
    }



    T_sp Vector_O::reverse()
    {_OF();
	int thisLength = this->length();
	Vector_sp newVec = eval::funcall(_sym_make_vector,this->_instanceClass()->className(),make_fixnum(thisLength)).as<T_O>().as<Vector_O>();
	for ( int i=0; i<thisLength; i++ )
	{
	    int ri = thisLength - i;
	    newVec->setf_elt(ri,this->elt(i));
	}
	return newVec;
    }

    /*! Return the reversed vector destructively modifying the current vector */
// Swaps at +
// Length = 6   halfLength = 3
//  0 1 2 3 4 5
//  + + + 
//  5 4 3 2 1 0
// Length = 5   halfLength = 2
//  0 1 2 3 4
//  + + 
//  4 3 2 1 0 
    T_sp Vector_O::nreverse()
    {_OF();
	int thisLength = this->length();
	int halfLength = thisLength/2;  // 5/2 = 2  0 
	T_sp temp;
	for ( int i=0; i<halfLength; i++ )
	{
	    int ri = thisLength-i;
	    this->swapElements(i,ri);
	}
	return this->sharedThis<T_O>();
    }


    
    
#define ARGS_cl_vectorPush "(newElement vector)"
#define DECL_cl_vectorPush ""
#define DOCS_cl_vectorPush "vectorPush"
    Fixnum_sp cl_vectorPush(T_sp newElement, Vector_sp vec)
    {_G();
        return vec->vectorPush(newElement);
    };



    
    
#define ARGS_cl_vectorPushExtend "(newElement vector &optional (exension 16))"
#define DECL_cl_vectorPushExtend ""
#define DOCS_cl_vectorPushExtend "vectorPushExtend"
    Fixnum_sp cl_vectorPushExtend(T_sp newElement, Vector_sp vec, int extension)
    {_G();
	return vec->vectorPushExtend(newElement,extension);
    }




    EXPOSE_CLASS(core,Vector_O);

    void Vector_O::exposeCando(::core::Lisp_sp lisp)
    {_G();
	::core::class_<Vector_O>()
	      .def("adjustableArrayP",&Vector_O::adjustableArrayP)
//	.initArgs("(self)")
	;
	SYMBOL_SC_(CorePkg,make_vector);
	Defun(make_vector);
	SYMBOL_EXPORT_SC_(CorePkg,adjustVector);
	CoreDefun(adjustVector);
        SYMBOL_EXPORT_SC_(ClPkg,vectorPush);
        ClDefun(vectorPush);
        SYMBOL_EXPORT_SC_(ClPkg,vectorPushExtend);
        ClDefun(vectorPushExtend);
        ClDefun(vector);
    }

    void Vector_O::exposePython(::core::Lisp_sp lisp)
    {
//	PYTHON_CLASS_2BASES(Pkg(),Vector,"","",_LISP)
#ifdef USEBOOSTPYTHON
    boost::python::class_< Vector_O,
			   gctools::smart_ptr< Vector_O >,
	    boost::python::bases< Vector_O::Bases::Base1, Vector_O::Bases::Base2>, 
	boost::noncopyable > ( "Vector_O", boost::python::no_init )
//	.initArgs("(self)")
	;
#endif
    }
    

}; /* core */
