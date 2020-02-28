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

#if !defined(__CLASP_CORE_EXTERNAL_OBJECT_H__)
#define __CLASP_CORE_EXTERNAL_OBJECT_H__ __FILE__" $Id"

#include <clasp/core/object.h>

namespace core {

  typedef enum {
    None = 0,
    DeleteOnDtor = 1,
    Copyable = 2,
  } ForeignDataFlagEnum;

// set this class up by hand
  SMART(ExternalObject);
  class ExternalObject_O : public General_O {
    LISP_CLASS(core,CorePkg,ExternalObject_O,"ExternalObject",General_O);

  private:
    Instance_sp _Class;

  public:
    virtual bool eql_(T_sp obj) const;
    CL_DEFMETHOD virtual bool isUndefined() const { return this->externalObject() == NULL; };
    virtual void *externalObject() const {
      SUBCLASS_MUST_IMPLEMENT();
    };
    virtual void set_externalObject(void *ptr) {
      SUBCLASS_MUST_IMPLEMENT();
    };

  public:
    explicit ExternalObject_O() : Base(), _Class(_Nil<Instance_O>()){};
    virtual ~ExternalObject_O(){};
  };

  template <class Type, class WrapperType>
    gctools::smart_ptr<Type> RP_Create_wrapped(WrapperType ptr) {
    GC_ALLOCATE(Type, wrapper);
    wrapper->set_wrapped(ptr);
    return wrapper;
  };

}; // namespace core

// public:

#ifndef SCRAPING

#define LISP_EXTERNAL_CLASS(oNamespace, oPackage, wrappedClass, aClass, nameOfWrappedClass, aClassBase) \
public:                                                                                                 \
  typedef aClassBase Base;                                                                              \
  typedef LispBases1<Base> Bases;                                                                       \
  COMMON_CLASS_PARTS(oNamespace, oPackage, aClass, nameOfWrappedClass)                                  \
  static gctools::smart_ptr<aClass> create() {                                                          \
    return gctools::GC<aClass>::allocate_with_default_constructor();                                    \
  };                                                                                                    \
  virtual core::Instance_sp __class() const {                           \
    return core::lisp_getStaticClass(aClass::static_StampWtagMtag);       \
  }                                                                     \
  typedef wrappedClass WrappedClass;                                                                    \
                                                                                                        \
public:                                                                                                 \
  /*Derived from StandardObject so it supports slots*/                                                  \
  static bool static_supportsSlots() { return true; }; 

#endif // SCRAPING

#endif // __CLASP_CORE_EXTERNAL_OBJECT_H__
