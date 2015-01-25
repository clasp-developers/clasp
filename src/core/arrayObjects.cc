/*
    File: arrayObjects.cc
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
#include <clasp/core/arrayObjects.h>
#include <clasp/core/wrappers.h>
namespace core
{

// ----------------------------------------------------------------------
//



    EXPOSE_CLASS(core,ArrayObjects_O);


#define ARGS_ArrayObjects_O_make "(dimensions element-type initial-element)"
#define DECL_ArrayObjects_O_make ""
#define DOCS_ArrayObjects_O_make "make ArrayObjects args: dimensions element-type initial-element"
    ArrayObjects_sp ArrayObjects_O::make(T_sp dim_desig, T_sp elementType, T_sp initialElement)
    {_G();
        GC_ALLOCATE(ArrayObjects_O,array );
	array->_ElementType = elementType;
	Cons_sp dim;
	if ( af_atom(dim_desig) )
	{
	    int idim = dim_desig.as<Integer_O>()->as_int();
	    dim = Cons_O::create(Fixnum_O::create(idim));
	} else
	{
	    dim = dim_desig.as_or_nil<Cons_O>();
	}
	/* LongLongInt elements = */ array->setDimensions(dim,initialElement);
	return((array));
    }



    void ArrayObjects_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<ArrayObjects_O>()
	;
	Defun_maker(CorePkg,ArrayObjects);
    }

    void ArrayObjects_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,ArrayObjects,"","",_lisp)
	;
#endif
    }

#if defined(XML_ARCHIVE)
void ArrayObjects_O::archiveBase(::core::ArchiveP node)
    {
        this->Base::archiveBase(node);
	IMPLEMENT_ME();
#if 0
	node->attribute("elementType",this->_ElementType);
	node->archiveVector0<T_O>("contents",this->_Values);
#endif
	// Archive other instance variables here
    }
#endif // defined(XML_ARCHIVE)
#if defined(OLD_SERIALIZE)
    void ArrayObjects_O::serialize(serialize::SNode node)
    {
        this->Base::serialize(node);
	node->namedObject("elementType",this->_ElementType);
	node->namedVector0<T_O>("contents",this->_Values);
    }
#endif


    void ArrayObjects_O::initialize()
    {_OF();
        this->Base::initialize();
	this->_ElementType = cl::_sym_T_O;
    }





    void ArrayObjects_O::rowMajorAset(int idx, T_sp value)
    {_G();
	ASSERTF(idx<this->_Values.size(),BF("Illegal row-major-aref index %d - must be less than %d") % idx % this->_Values.size() );
	this->_Values[idx] = value;
    }

    T_sp ArrayObjects_O::asetUnsafe(int idx, T_sp value)
    {_G();
	this->_Values[idx] = value;
	return value;
    }

    T_sp ArrayObjects_O::rowMajorAref(int idx) const
    {_G();
	ASSERTF(idx<this->_Values.size(),BF("Illegal row-major-aref index %d - must be less than %d") % idx % this->_Values.size() );
	return((this->_Values[idx]));
    };







    T_sp ArrayObjects_O::aref(Cons_sp indices) const
    {_OF();
	LOG(BF("indices[%s]")%_rep_(indices) );
	int index = this->index(indices);
	return((this->_Values[index]));
    }

    T_sp ArrayObjects_O::setf_aref(Cons_sp indices_val)
    {_OF();
	Cons_sp val_cons;
	LongLongInt index = this->index_val(indices_val,true,val_cons);
	this->_Values[index] = oCar(val_cons);
	return((oCar(val_cons)));
    }

    T_sp ArrayObjects_O::shallowCopy() const
    {_OF();
        GC_ALLOCATE(ArrayObjects_O,array );
	array->_Dimensions = this->_Dimensions;
	array->_ElementType = this->_ElementType;
	array->_Values = this->_Values;
	return((array));
    }

    void ArrayObjects_O::arrayFill(T_sp val)
    {_OF();
	for ( int i=0; i<(int)this->_Values.size(); i++ )
	{
	    this->_Values[i] = val;
	}
    }


    T_sp ArrayObjects_O::deepCopy() const
    {_OF();
        GC_ALLOCATE(ArrayObjects_O,narray );
        narray->_Dimensions = this->_Dimensions;
	narray->_ElementType = this->_ElementType; // Don't copy ElementType - it's an immutable Symbol representing a Class
	narray->_Values.resize(this->_Values.size());
	for ( uint i=0; i<this->_Values.size(); i++ )
	{
	    narray->_Values[i] = this->_Values[i]->deepCopy();
	}
	return((narray));
    }


    T_sp ArrayObjects_O::svref(int index) const
    {_G();
	if ( this->_Dimensions.size() == 1 )
	{
	    ASSERT(index>=0 && index < this->_Dimensions[0]);
	    return((this->_Values[index]));
	}
	SIMPLE_ERROR(BF("ArrayObjects has more than one dimension - cannot use svref"));
    }



    T_sp ArrayObjects_O::setf_svref(int index, T_sp value)
    {_G();
	if ( this->_Dimensions.size() == 1 )
	{
	    ASSERT(index>=0 && index < this->_Dimensions[0]);
	    this->_Values[index] = value;
	    return((value));
	}
	SIMPLE_ERROR(BF("ArrayObjects has more than one dimension - cannot use setf-svref"));
    }


    LongLongInt ArrayObjects_O::setDimensions(Cons_sp dim,T_sp initialElement)
    {_OF();
	LongLongInt elements = 1;
	this->_Dimensions.resize(cl_length(dim));
	int idx = 0;
	for ( ; dim.notnilp(); dim = cCdr(dim) )
	{
	    int oneDim = oCar(dim).as<Rational_O>()->as_int();
	    this->_Dimensions[idx] = oneDim;
	    elements *= oneDim;
	    idx++;
	}
	this->_Values.resize(elements,initialElement);
	return((elements));
    }


    int ArrayObjects_O::arrayDimension(int axisNumber) const
    {_OF();
	ASSERTF(axisNumber>=0,BF("Axis number must be >= 0"));
	ASSERTF(axisNumber<(int)this->_Dimensions.size(),BF("There is no axis with number %d - must be less than %d")
		% axisNumber % this->_Dimensions.size() );
	return((this->_Dimensions[axisNumber]));
    }



#if defined(XML_ARCHIVE)
void ArrayObjects_O::archiveBase(::core::ArchiveP node)
    {
        this->Base::archiveBase(node);
	node->archiveVectorInt("dims",this->_Dimensions);
	// Archive other instance variables here
    }
#endif // defined(XML_ARCHIVE)
#if defined(OLD_SERIALIZE)
    void ArrayObjects_O::serialize(serialize::SNode node)
    {
	node->namedPOD("dims",this->_Dimensions);
    }
#endif



}; /* core */
