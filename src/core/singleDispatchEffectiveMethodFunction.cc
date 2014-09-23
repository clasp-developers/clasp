/*
    File: singleDispatchEffectiveMethodFunction.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
#include "symbolTable.h"
#include "singleDispatchMethod.h"
#include "singleDispatchEffectiveMethodFunction.h"
#include "core/wrappers.h"
namespace core
{
    
// ----------------------------------------------------------------------
//
    
    EXPOSE_CLASS(core,SingleDispatchEffectiveMethodFunction_O);
    
    void SingleDispatchEffectiveMethodFunction_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<SingleDispatchEffectiveMethodFunction_O>()
//	.initArgs("(self)")
	    ;
    }
    
    void SingleDispatchEffectiveMethodFunction_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),SingleDispatchEffectiveMethodFunction,"","",_LISP)
//	.initArgs("(self)")
	    ;
#endif
    }
    
    
#if 0
#if defined(OLD_SERIALIZE)
    void SingleDispatchEffectiveMethodFunction_O::serialize(::serialize::SNodeP node)
    {
	IMPLEMENT_ME();
        this->Bases::serialize(node);
	// Archive other instance variables here
    }
#endif
    
#if defined(XML_ARCHIVE)
    void SingleDispatchEffectiveMethodFunction_O::archiveBase(::core::ArchiveP node)
    {
	IMPLEMENT_ME();
        this->Base1::archiveBase(node);
	// Archive other instance variables here
    }
#endif // defined(XML_ARCHIVE)
#endif
    

    SingleDispatchEffectiveMethodFunction_sp SingleDispatchEffectiveMethodFunction_O::create(Cons_sp methods, Lisp_sp lisp)
    {_G();
        GC_ALLOCATE(SingleDispatchEffectiveMethodFunction_O,emf );
	    emf->_Methods = methods;
	return emf;
    }


    void SingleDispatchEffectiveMethodFunction_O::initialize()
    {_OF();
        this->Base::initialize();
    }


    string SingleDispatchEffectiveMethodFunction_O::__repr__() const
    {_OF();
	stringstream ss;
	for (Cons_sp cur=this->_Methods; cur.notnilp(); cur=cCdr(cur))
	{
	    ss << "method has Receiver class[" << _rep_(oCar(cur).as<SingleDispatchMethod_O>()->receiver_class()) << "]" << std::endl;
	}
	return ss.str();
    }



    void SingleDispatchEffectiveMethodFunction_O::LISP_INVOKE()
    {_OF();
	LOG(BF("My methods --> %s") % _rep_(this) );
	IMPLEMENT_MEF(BF("Implement emf"));
    }

    
    
}; /* core */
