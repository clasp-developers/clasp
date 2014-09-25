/*
    File: weakPointer.cc
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

#include "core/common.h"
#include "core/environment.h"
#include "weakPointer.h"
#include "core/wrappers.h"
namespace core
{

// ----------------------------------------------------------------------
//

#define ARGS_WeakPointer_O_make "(obj)"
#define DECL_WeakPointer_O_make ""
#define DOCS_WeakPointer_O_make "make WeakPointer args: obj"
    WeakPointer_sp WeakPointer_O::make(T_sp obj)
    {_G();
        GC_ALLOCATE_VARIADIC(WeakPointer_O,me,obj);
	return me;
    };


    EXPOSE_CLASS(core,WeakPointer_O);

    void WeakPointer_O::exposeCando(Lisp_sp lisp)
    {
	class_<WeakPointer_O>()
	    .def("weakPointerValid",&WeakPointer_O::valid)
	    .def("weakPointerValue",&WeakPointer_O::value)
	;
	Defun_maker(CorePkg,WeakPointer);
       
    }

    void WeakPointer_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,WeakPointer,"","",_lisp)
	    .def("weakPointerValid",&WeakPointer_O::valid)
	    .def("weakPointerValue",&WeakPointer_O::value)
	;
#endif
    }






#if defined(OLD_SERIALIZE)
    void WeakPointer_O::serialize(serialize::SNode snode)
    {
	CR_HINT(snode,false);
	snode->archiveWeakPointer("weakObject",this->_WeakObject);
	CR_HINT(snode,false);
    }
#endif // defined(OLD_SERIALIZE)

#if defined(XML_ARCHIVE)
    void WeakPointer_O::archiveBase(ArchiveP node)
    {
        this->Base::archiveBase(node);
	node->archiveWeakPointer("weakObject",this->_WeakObject);
    }
#endif // defined(XML_ARCHIVE)


    bool WeakPointer_O::valid() const
    {
        return this->_WeakObject.valid();
    }

    /*! Return (values value t) or (values nil nil) */
    T_mv WeakPointer_O::value() const
    {
        return this->_WeakObject.value();
    }


    

}; /* core */
