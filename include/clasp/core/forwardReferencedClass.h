/*
    File: forwardReferencedClass.h
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
#ifndef _core_ForwardReferencedClass_H
#define _core_ForwardReferencedClass_H

#include <clasp/core/foundation.h>
#include <clasp/core/metaClass.h>

namespace core {

FORWARD(ForwardReferencedClass);

/*! Clisp says that forward-referenced-class should not be a subclass of class and specializer but should be a subclass of metaobject
 * google: forward-referenced-class clisp
 http://clisp.podval.org/impnotes/mop-overview.html#forward-referenced-class-clisp
*/
class ForwardReferencedClass_O : public Class_O {
  LISP_META_CLASS(StandardClass);
  LISP_BASE1(Class_O);
  LISP_CLASS(core, CorePkg, ForwardReferencedClass_O, "ForwardReferencedClass");
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(ForwardReferencedClass_O);

public:
  void initialize();

GCPRIVATE: // instance variables here
  BuiltInClass_sp _InstanceCoreClass;

public: // Functions here
  void setInstanceCoreClass(BuiltInClass_sp bic);

  void defineYourSlotsFromBinderArchiveNode(ArchiveP binderNode);
};

}; /* core */

TRANSLATE(core::ForwardReferencedClass_O);
template <>
struct gctools::GCInfo<core::ForwardReferencedClass_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

#endif /* _core_ForwardReferencedClass_H */
