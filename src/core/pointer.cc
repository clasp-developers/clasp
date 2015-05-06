/*
    File: pointer.cc
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
#include <clasp/core/common.h>
#include <clasp/core/pointer.h>
#include <clasp/core/wrappers.h>

namespace core
{

    void Pointer_O::initialize()
    {
	this->Base::initialize();
	this->_Pointer = NULL;
    }


    Pointer_sp Pointer_O::create(void* p)
    {_G();
        GC_ALLOCATE(Pointer_O,ptr );
	    ptr->_Pointer = p;
	return ptr;
    }


    EXPOSE_CLASS(core,Pointer_O);

    void Pointer_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<Pointer_O>()
	    ;
    }

    void Pointer_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Pointer,"","",_lisp)
	    ;
#endif
    }

    bool Pointer_O::eql_(T_sp obj) const
    {
        if ( this->eq(obj) ) return true;
        if ( Pointer_sp pobj = obj.asOrNull<Pointer_O>() ) {
            return (this->_Pointer == pobj->_Pointer);
        }
        return false;
    }

    string Pointer_O::__repr__() const
    {
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() << " :ptr " << (BF("%p") % this->_Pointer).str() << ">";
	return ss.str();
    }



};
