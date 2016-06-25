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


#if !defined( __CLASP_CORE_EXTERNAL_OBJECT_H__ )
#define __CLASP_CORE_EXTERNAL_OBJECT_H__

// ---------------------------------------------------------------------------
//    SYSTEM INCLUDES
// ---------------------------------------------------------------------------

// --- Standard C++ Includes ---
// NONE

// --- Platform-specific Includes ---
// NONE

// ---------------------------------------------------------------------------
//    APPLICaATION INCLUDES
// ---------------------------------------------------------------------------

#include "clasp/core/foundation.h"
#include "clasp/core/object.h"
#include "clasp/core/standardObject.h"

#if defined( __cplusplus )

// ---------------------------------------------------------------------------
//   NAMESPACE
// ---------------------------------------------------------------------------

namespace core {

// ---------------------------------------------------------------------------
//   CLASSES & METHODS & FUNCTIONS
// ---------------------------------------------------------------------------

// set this class up by hand
  SMART(ExternalObject);
  class ExternalObject_O : public General_O
  {
    LISP_CLASS(core, CorePkg, ExternalObject_O, "ExternalObject",General_O);
  private:
    Class_sp _Class;

  public:
    virtual bool eql_(T_sp obj) const;
    CL_LISPIFY_NAME("isUndefined");
    CL_DEFMETHOD   virtual bool isUndefined() const { return this->externalObject() == NULL; };
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

  template <class Type, class WrapperType>
    gctools::smart_ptr<Type> RP_Create_wrapped(WrapperType ptr) {
    GC_ALLOCATE(Type, wrapper);
    wrapper->set_wrapped(ptr);
    return wrapper;
  }
};

// public:

#ifndef SCRAPING
#define LISP_EXTERNAL_CLASS(oNamespace, oPackage, wrappedClass, aClass, nameOfWrappedClass, aClassBase) \
  public:                                                               \
  typedef aClassBase Base;                                            \
  typedef LispBases1<Base> Bases;                                       \
  COMMON_CLASS_PARTS(oNamespace, oPackage, aClass, nameOfWrappedClass)  \
  static gctools::smart_ptr<aClass> create() {                          \
    return gctools::GC<aClass>::allocate_with_default_constructor();  \
  };                                                                  \
  typedef wrappedClass WrappedClass;                                    \
public:                                                                \
 /*Derived from StandardObject so it supports slots*/                  \
 static bool static_supportsSlots() { return true; };                  \
 virtual core::Class_sp __class() const {                              \
   return aClass::static_class;                                        \
 }
  /* end */

#endif // SCRAPING

namespace core {

  typedef enum { DeleteOnDtor = 1,
                 Copyable = 2,
  } ForeignDataFlagEnum;

// set this class up by hand
  SMART(ForeignData);
/* Maintain a pointer to a block of Foreign data that we may or may not own depending on _OwnershipFlags */

  class ForeignData_O : public ExternalObject_O
  {
    LISP_CLASS(core, CorePkg, ForeignData_O, "ForeignData",ExternalObject_O);
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


template <>
struct gctools::GCInfo<core::ForeignData_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

#endif // __cplusplus
#endif // __CLASP_CORE_EXTERNAL_OBJECT_H__
