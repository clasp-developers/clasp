/*
    File: ql.h
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
#ifndef	_core_quickList_H
#define	_core_quickList_H

#include <clasp/core/sourceFileInfo.h>

/*! Handle template class for creating Lisp Cons's on the fly from within C++ and
  applying them to primitive functions */
namespace ql
{


/*! list is a class for constructing free-form Cons lists
  from within C++.
  
  eg:
  C++: (ql::list(lisp) , x , y , z , _lisp->symbol(kw::_sym_color) , objBlue ).cons() 
  returns --> (list X Y Z :color BLUE )   where X,Y,Z,BLUE are the objects contained by the
  C++ values x, y, z, objBlue which are of type T_sp
*/



    class list : public gctools::StackBoundClass
    {
    private:
	core::Cons_sp _First; // ROOT
	core::Cons_sp _Tail; // ROOT 
    public:
	/*! ctor sets up _Lisp and the first element of the Cons */
    list(core::Lisp_sp lisp)
	{
	    this->clear();
	}

	list()
	{_G();
	    this->clear();
	}

	/*! Point _First of this list to the _Tail of another list */
	void point_to_tail(ql::list const& other)
	{_G();
	    this->_First = other._Tail;
	    this->_Tail = this->_First;
	}

	void clear()
	{_G();
	    this->_First = core::Cons_O::create(_Nil<core::T_O>());
	    this->_Tail = this->_First;
	}

	void create_from_cons(core::List_sp other)
	{_G();
	    this->clear();
	    for ( auto cur : other ) {
		this->operator<<(oCar(cur));
	    }
	}
	
	core::Cons_sp tail() const
	{
	    return this->_Tail;
	}

	int length() const
	{
	    return this->_First->length()-1;
	};

	inline list& operator<<(core::T_sp const& obj)
	{
	    this->throwIfClosed();
	    core::Cons_sp one = core::Cons_O::create(obj);
	    this->_Tail->setCdr(one);
	    this->_Tail = one;
	    return *this;
	}

	/*! Insert list into list - should I copy or append (which will modify the argument)? */
	inline list& operator&(core::List_sp l)
	{
	    this->throwIfClosed();
	    for ( auto cur : l ) {
		(*this) << oCar(cur);
	    }
	    return *this;
	}

	/*! Handle automatic conversion from a c++ int to Fixnum_O */

	inline list& operator<<(int const& val)
	{

	    return ( (*this) << core::make_fixnum(val));
	}
      

	/*! dot the list argument to the end of the list */
	inline list& dot(core::T_sp arg)
	{
	    this->throwIfClosed();
	    this->_Tail->setCdr(arg);
	    return *this;
	}
	
	inline core::List_sp cons() const
	{
	    return coerce_to_list(oCdr(this->_First));
	}

	/*! Return all of the list including the (usually) dummy first element */
	inline core::List_sp all() const
	{
	    return coerce_to_list(this->_First);
	}


	inline void throwIfClosed() const
	{
	    if ( oCdr(this->_Tail).notnilp() )
	    {
		THROW_HARD_ERROR(BF("You tried to modify a closed list"));
	    }
	};
    };





#ifdef USE_SOURCE_CODE_CONS
    class source_code_list
    {
    private:
	core::SourceCodeCons_rp _First; // ROOT
	core::SourceCodeCons_rp _Tail; // ROOT 
    public:
	/*! ctor sets up _Lisp and the first element of the Cons */
	source_code_list()
	{_G();
	    this->_First = core::SourceCodeCons_O::create(_Nil<core::T_O>(),_Nil<core::Cons_O>(),0,0,_Nil<core::SourceFileInfo_O>(),_lisp);
	    this->_Tail = this->_First;
	}

	/*! ctor sets up _Lisp and the first element of the SourceCodeCons.
	 It sets the lineNumber/column/fileName from a stream */
	source_code_list(uint lineNumber, uint column, core::SourceFileInfo_sp fileName)
	{
	    this->_First = core::SourceCodeCons_O::create(_Nil<core::T_O>(),_Nil<core::Cons_O>(),lineNumber,column,fileName,_lisp);
	    this->_Tail = this->_First;
	}


	/*! ctor sets up the first element of the SourceCodeCons .
	 It sets the lineNumber/column/fileName from the sourceCodeTemplate */
	source_code_list(core::Cons_sp sourceCodeTemplate)
	{
	    this->_First = core::SourceCodeCons_O::createWithDuplicateSourceCodeInfo(_Nil<core::T_O>(),sourceCodeTemplate,_lisp);
	    this->_Tail = this->_First;
	}

	inline source_code_list& operator<<(core::T_rp const& obj)
	{
	    this->throwIfClosed();
	    core::SourceCodeCons_rp one = core::SourceCodeCons_O::createWithDuplicateSourceCodeInfo(obj,this->_First,_lisp);
	    this->_Tail->setCdr(one);
	    this->_Tail = one;
	    return *this;
	}

	inline source_code_list& appendWithParsePos(core::T_sp obj, core::Cons_sp parsePosTemplate)
	{
	    this->throwIfClosed();
	    core::SourceCodeCons_rp one = core::SourceCodeCons_O::createWithDuplicateSourceCodeInfo(obj,parsePosTemplate,_lisp);
	    this->_Tail->setCdr(one);
	    this->_Tail = one;
	    return *this;
	}


	inline source_code_list& set_tail_source_info(core::Cons_sp sourceInfoTemplate)
	{
	    this->_Tail->duplicateSourceCodeInfo(sourceInfoTemplate);
	    return *this;
	}

	inline source_code_list& dot(core::T_sp arg)
	{
	    this->throwIfClosed();
	    this->_Tail->setCdr(arg);
	    return *this;
	}


	inline core::Cons_rp cons() const
	{
	    return cCdr(this->_First);
	}


	inline void throwIfClosed() const
	{
	    if ( oCdr(this->_Tail).notnilp() )
	    {
		THROW_HARD_ERROR(BF("You tried to modify a closed list"));
	    }
	};
    };
#endif



/*
  Later I want to add support to apply c++ functions that have been fbound 

  apply::apply(func,(apply::list(_lisp) % x % y % z % _lisp->symbol(kw::_sym_rest) % stuff ).cons() )
  apply::apply(func,(apply::list(_lisp) % x % y % z % _lisp->symbol(kw::_sym_rest) % stuff ).cons(tail) )

*/

};

#endif // _core_quickList_H
