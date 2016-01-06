/*
    File: forwardReferencedClass.cc
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
#include <clasp/core/forwardReferencedClass.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

EXPOSE_CLASS(core, ForwardReferencedClass_O);

void ForwardReferencedClass_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<ForwardReferencedClass_O>()
      ;
}

void ForwardReferencedClass_O::exposePython(::core::Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(Pkg(), ForwardReferencedClass, "", "", _LISP)
      //	.initArgs("(self)")
      ;
#endif
}

#if 0
    void ForwardReferencedClass_O::serialize(::serialize::SNodeP node)
    {
	IMPLEMENT_ME();
        this->Bases::serialize(node);
	// Archive other instance variables here
    }

    void ForwardReferencedClass_O::archiveBase(::core::ArchiveP node)
    {
	IMPLEMENT_ME();
        this->Base1::archiveBase(node);
	// Archive other instance variables here
    }
#endif

void ForwardReferencedClass_O::initialize() {
  _OF();
  this->Base::initialize();
  this->_InstanceCoreClass = _Nil<BuiltInClass_O>();
}

void ForwardReferencedClass_O::setInstanceCoreClass(BuiltInClass_sp bic) {
  _OF();
  this->_InstanceCoreClass = bic;
}

void ForwardReferencedClass_O::defineYourSlotsFromBinderArchiveNode(ArchiveP node) {
  _OF();
  IMPLEMENT_MEF(BF("Implement %s") % __FUNCTION__);
}

}; /* core */
