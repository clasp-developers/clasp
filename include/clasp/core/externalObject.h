/*
    File: externalObject.h
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
#ifndef ExternalObject_H
#define ExternalObject_H

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/standardObject.h>

#if 0
namespace core
{
    SMART(ExternalObjectManager);
    c l a s s ExternalObjectManager_O : public T_O
    {
	L I S P _BASE1(T_O);
	L I S P _CLASS(core,CorePkg,ExternalObjectManager_O,"ExternalObjectManager");
	void	initialize();
    private:
	map<void*,ExternalObject_sp>	_ExternalPointersToObjects;
    public:
	void registerExternal(void* ptr, ExternalObject_sp obj, Lisp_sp lisp);
	bool recognizesExternal(void* ptr);
	ExternalObject_sp objectForExternal(void* ptr);

	DEFAULT_CTOR_DTOR(ExternalObjectManager_O);
    };
TRANSLATE(core::ExternalObjectManager_O);

};
#endif

namespace core {

// set this class up by hand
SMART(ExternalObject);
class ExternalObject_O : public T_O // StandardObject_O
                         {
  LISP_BASE1(T_O); // LISP_BASE1(StandardObject_O);
  LISP_CLASS(core, CorePkg, ExternalObject_O, "ExternalObject");
GCPRIVATE:
  Class_sp _Class;

public:
  virtual bool eql_(T_sp obj) const;
  virtual bool isUndefined() const { return this->externalObject() == NULL; };
  virtual void *externalObject() const {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  };
  virtual void set_externalObject(void *ptr) {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  };

public:
  explicit ExternalObject_O() : Base(), _Class(_Nil<Class_O>()){};
  virtual ~ExternalObject_O(){};
};

template <class OT, class WT>
gctools::smart_ptr<OT> RP_Create_wrapped(WT ptr) {
  _G();
  GC_ALLOCATE(OT, wrapper);
  wrapper->set_wrapped(ptr);
  return wrapper;
}

// public:

#define LISP_EXTERNAL_CLASS(oNamespace, oPackage, wrappedClass, o_nameOfWrappedClass, nameOfWrappedClass, o_nameOfWrappedClassBase)      \
  /* */ LISP_BASE1(o_nameOfWrappedClassBase);                                                                                            \
  /* */ __COMMON_CLASS_PARTS(oNamespace, oPackage, o_nameOfWrappedClass, nameOfWrappedClass) public : typedef wrappedClass WrappedClass; \
                                                                                                                                         \
public:                                                                                                                                  \
  /*Derived from StandardObject so it supports slots*/                                                                                   \
  static bool static_supportsSlots() { return true; };                                                                                   \
  /* end */
};

TRANSLATE(core::ExternalObject_O);

namespace core {

typedef enum { DeleteOnDtor = 1,
               Copyable = 2,
} ForeignDataFlagEnum;

// set this class up by hand
SMART(ForeignData);
/* Maintain a pointer to a block of Foreign data that we may or may not own depending on _OwnershipFlags */

class ForeignData_O : public ExternalObject_O // StandardObject_O
                      {
  LISP_BASE1(ExternalObject_O); // LISP_BASE1(StandardObject_O);
  LISP_CLASS(core, CorePkg, ForeignData_O, "ForeignData");
#if defined(XML_ARCHIVE)
  void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
       //	string	__repr__() const;
  T_sp _Kind;
  int _OwnershipFlags;
  size_t _Size;
  void *_Data;

public:
  static ForeignData_sp allocateForeignObject(T_sp kind);

public:
  template <class T>
  T data() { return reinterpret_cast<T>(this->_Data); };

private:
  void allocate(T_sp kind, int ownershipFlags, size_t size);

  void freeForeignObject();

public:
  explicit ForeignData_O();
  virtual ~ForeignData_O(); // non-trivial
};
};

TRANSLATE(core::ForeignData_O);

template <>
struct gctools::GCInfo<core::ForeignData_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

#endif
