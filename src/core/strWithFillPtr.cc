#define	DEBUG_LEVEL_FULL

#include "clasp_gmpxx.h"
#include <ctype.h>
#include "core/common.h"
#include "core/environment.h"
#include "symbolTable.h"
#include "core/designators.h"
#include "core/character.h"
#include "bignum.h"
#include "multipleValues.h"
#include "strWithFillPtr.h"
#include "print.h"
#include "lispStream.h"
#include "hashTable.h"
#include "core/wrappers.h"

namespace core
{

    StrWithFillPtr_sp StrWithFillPtr_O::create(char initial_element, int dimension, int fill_ptr, bool adjustable, Sequence_sp initialContents)
    {_G();
        GC_ALLOCATE(StrWithFillPtr_O,str );
	str->_Contents = string(dimension, initial_element);
	str->_FillPointer = fill_ptr;
	str->_Adjustable = adjustable;
	if ( initialContents.notnilp() )
	{
	    str->fillInitialContents(initialContents);
	}
	return str;
    }

    EXPOSE_CLASS(core,StrWithFillPtr_O);

	void StrWithFillPtr_O::exposeCando(Lisp_sp lisp)
	{_G();
	    class_<StrWithFillPtr_O>()
		.def("core:pushString",(void (StrWithFillPtr_O::*)(T_sp))&StrWithFillPtr_O::pushString)
		;
	}


    void StrWithFillPtr_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Str,"","",_lisp)
		;
#endif
	}


    Fixnum_sp StrWithFillPtr_O::vectorPush(T_sp newElement)
    {
	if ( newElement.notnilp() )
	{
	    if ( Character_sp ch = newElement.asOrNull<Character_O>() )
	    {
		if ( this->_FillPointer < this->_Contents.size() )
		{
		    this->_Contents[this->_FillPointer] = ch->asChar();
		    int newIndex = this->_FillPointer;
		    this->_FillPointer++;
		    return Fixnum_O::create(newIndex);
		}
		return _Nil<Fixnum_O>();
	    }
	}
	TYPE_ERROR(newElement,cl::_sym_Character_O);
    }

    void StrWithFillPtr_O::setFillPointer(size_t fp)
    {
	if (fp < this->_Contents.size() )
	{
	    this->_FillPointer = fp;
	    return;
	}
	SIMPLE_ERROR(BF("Illegal fill-pointer %d - must be less than %d") % fp % this->_Contents.size() );
    }

    void StrWithFillPtr_O::adjustSize(int extension)
    {
	this->setSize(this->_Contents.size()+extension);
    }

    void StrWithFillPtr_O::setSize(int newSize)
    {
	if ( this->_Adjustable ) {
	    if ( newSize < 0 ) {
		SIMPLE_ERROR(BF("You can only set size to >= 0 - not %d") % newSize);
	    }
	    if ( this->_Contents.size() == newSize ) return;
	    if ( this->_Contents.size() < newSize ) {
                this->_Contents.resize(newSize,' ');
		return;
	    } else {
		this->_Contents.resize(newSize);
		if ( this->_FillPointer > this->_Contents.size() ) {
		    this->_FillPointer = this->_Contents.size();
		}
		return;
	    }
	}
	SIMPLE_ERROR(BF("The array is not adjustable"));
    }


    int	StrWithFillPtr_O::pushCharExtend(brclChar c, int extension)
    {
	if ( this->_FillPointer >= this->_Contents.size() )
	{
	    if (extension == 0 ) extension = this->_Contents.size();
	    this->adjustSize(extension);
	}
	this->_Contents[this->_FillPointer] = c;
	int newIndex = this->_FillPointer;
	this->_FillPointer++;
	return newIndex;
    }


    Fixnum_sp StrWithFillPtr_O::vectorPushExtend(T_sp newElement, int extension)
    {
	if ( newElement.notnilp() )
	{
	    if ( Character_sp ch = newElement.asOrNull<Character_O>() )
	    {
		return Fixnum_O::create(this->pushCharExtend(ch->asChar(),extension));
	    }
	}
	TYPE_ERROR(newElement,cl::_sym_Character_O);
    }


    string StrWithFillPtr_O::__repr__() const
    {
	stringstream ss;
	ss << '"';
	int i = 0;
	int iEnd(this->_FillPointer);
	const char* cur=this->_Contents.c_str(); // this->_Contents is a std::string
	while (i<iEnd)
	{
	    if ( *cur == '"')
	    {
		ss << '\\' << '"';
	    } else if ( *cur == '\n' )
	    {
		ss << '\\' << 'n';
	    } else
	    {
		ss << *cur;
	    }
	    ++cur;
	    ++i;
	}
	ss << '"';
	return ss.str();
    }



     void StrWithFillPtr_O::pushSubString(T_sp tstr, size_t start, size_t end)
     {
	 Str_sp str = af_string(tstr);
	 while (start < end) {
	     this->vectorPushExtend( Character_O::create(af_char(str, start)));
	     start++;
	 }
    }

    void StrWithFillPtr_O::pushString(T_sp str) {
	this->pushSubString( str, 0, af_length(str));
    }


    void StrWithFillPtr_O::pushString(const char* cPtr)
    {
	while (*cPtr) {
	    this->pushCharExtend(*cPtr, this->length());
	    cPtr++;
	}
    }



    char* StrWithFillPtr_O::addressOfFillPtr() 
    {
	return static_cast<char*>(this->addressOfBuffer())+this->_FillPointer;
    }

    void StrWithFillPtr_O::incrementFillPointer(int off)
    {
	this->_FillPointer += off;
    }


    void StrWithFillPtr_O::ensureSpaceAfterFillPointer(size_t size)
    {
	int left = this->_Contents.size()-this->_FillPointer;
	if (left < size ) {
	    this->adjustSize(size-left);
	}
    }

	
    



}; /* core */
