/*
    File: externalObject.cc
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

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/externalObject.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lisp.h>

// last include is wrappers.h
#include <clasp/core/wrappers.h>

namespace core {

#if 0
    EXPOSE_CLASS(core,ExternalObjectManager_O);
    void ExternalObjectManager_O::exposeCando(Lisp_sp env)
    {
        class_<ExternalObjectManager_O>()
            ;
    }

    void ExternalObjectManager_O::exposePython(Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,ExternalObjectManager,"","",_lisp)
            ;
#endif
    }
    void ExternalObjectManager_O::initialize()
    {
        this->Base::initialize();
        this->_ExternalPointersToObjects.clear();
    }


    void ExternalObjectManager_O::registerExternal(void* ptr, ExternalObject_sp obj,Lisp_sp lisp)
    {
        LOG(BF("Registering external ptr@%p to correspond to object(%s)") % ptr % _rep_(obj) );
        this->_ExternalPointersToObjects[ptr] = obj;
    }

    bool ExternalObjectManager_O::recognizesExternal(void* ptr)
    {
        return this->_ExternalPointersToObjects.count(ptr)>0;
    }

    ExternalObject_sp ExternalObjectManager_O::objectForExternal(void* ptr)
    {_OF();
        if ( !this->recognizesExternal(ptr) )
        {
            SIMPLE_ERROR(BF("The external pointer@%p is not recognized") % ptr );
        }
        if ( !this->_ExternalPointersToObjects[ptr] )
        {
            SIMPLE_ERROR(BF("Object for external ptr@p was deleted") % ptr );
        }
        return this->_ExternalPointersToObjects[ptr];
    }
#endif

void ExternalObject_O::exposeCando(Lisp_sp e) {
  class_<ExternalObject_O>()
      .def("isUndefined", &ExternalObject_O::isUndefined);
}

void ExternalObject_O::exposePython(Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON //[
  PYTHON_CLASS(CorePkg, ExternalObject, "", "", _lisp);
#endif //]
}

#if defined(XML_ARCHIVE)
void ExternalObject_O::archiveBase(ArchiveP node) {
  _OF();
  this->Base::archiveBase(node);
  IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)

bool ExternalObject_O::eql_(T_sp obj) const {
  if (core__external_object_p(obj)) {
    return (gc::As<ExternalObject_sp>(obj)->externalObject() == this->externalObject());
  }
  return false;
}

EXPOSE_CLASS(core, ExternalObject_O);

EXPOSE_CLASS(core, ForeignData_O);

#define ARGS_ForeignData_O_allocateForeignObject "(kind)"
#define DECL_ForeignData_O_allocateForeignObject ""
#define DOCS_ForeignData_O_allocateForeignObject "Allocate a chunk of memory for foreign-data"

ForeignData_sp ForeignData_O::allocateForeignObject(T_sp kind) {
  GC_ALLOCATE(ForeignData_O, obj);
  Cons_sp ckind = gc::As<Cons_sp>(kind);
  ASSERTF(oCar(ckind) == cl::_sym_array || oCar(ckind) == kw::_sym_array, BF("The first element of a foreign-data type must be ARRAY or :ARRAY"));
  ASSERTF(oCadr(ckind) == cl::_sym_UnsignedByte || oCadr(ckind) == kw::_sym_UnsignedByte, BF("The first element of a foreign-data type must be UNSIGNED-BYTE or :UNSIGNED-BYTE"));
  size_t size = unbox_fixnum(gc::As<Fixnum_sp>(oCaddr(ckind)));
  obj->allocate(kind, DeleteOnDtor, size);
  return obj;
}

void ForeignData_O::exposeCando(Lisp_sp lisp) {
  class_<ForeignData_O>()
      .def("freeForeignObject", &ForeignData_O::freeForeignObject);
  af_def(CurrentPkg, "allocateForeignObject", &ForeignData_O::allocateForeignObject, ARGS_ForeignData_O_allocateForeignObject, DECL_ForeignData_O_allocateForeignObject, DOCS_ForeignData_O_allocateForeignObject);
}

void ForeignData_O::exposePython(Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON //[
  PYTHON_CLASS(CorePkg, ForeignData, "", "", _lisp);
#endif //]
}

ForeignData_O::ForeignData_O() : _Kind(_Nil<T_O>()), _OwnershipFlags(0), _Size(0), _Data(NULL) {
}

void ForeignData_O::allocate(T_sp kind, int ownershipFlags, size_t size) {
  this->_Kind = kind;
  this->_OwnershipFlags = ownershipFlags;
  this->_Size = size;
  this->_Data = (void *)malloc(size);
}

CL_NAME("freeForeignObject");
CL_DEFMETHOD void ForeignData_O::freeForeignObject() {
  if (this->_Data) {
    free(this->_Data);
    this->_Data = NULL;
  }
}

ForeignData_O::~ForeignData_O() {
  if ((this->_OwnershipFlags & DeleteOnDtor) && this->_Data) {
    free(this->_Data);
    this->_Data = NULL;
  }
}
};
