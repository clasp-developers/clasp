/*
    File: structureClass.h
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
#ifndef _core_structureClass_H
#define _core_structureClass_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/holder.h>

namespace core {
  SMART(StructureClass);
};
template <>
struct gctools::GCInfo<core::StructureClass_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};


namespace core {
  SMART(StructureClass);
  SMART(StringSet);
  SMART(StructureClass);

 /*! A StructureClass maintains info about StructureObjects within the minimal Common Lisp
A StructureClass can inherit from one other class but can have any number of mixins */
  class StructureClass_O : public Class_O {
    LISP_META_CLASS(::_lisp->_Roots._StandardClass);
    LISP_CLASS(core, ClPkg, StructureClass_O, "structure-class",Class_O);
  public:
#if defined(XML_ARCHIVE)
    void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    void initialize();
  public:
//    Class_sp _InstanceCoreClass;
  public:
    static StructureClass_sp ensure_structure_class( Symbol_sp name, T_sp included_class, List_sp mixins);
  public:
    virtual void reinitialize_class();
  public:
  /*! Special creator used when starting up lisp environment, the object returned will be a root */
    static StructureClass_sp createUncollectable(gctools::Stamp is);
    explicit StructureClass_O(gctools::Stamp is, Class_sp metaclass, size_t slots) : Class_O(is,metaclass, slots) {};
  };
};


namespace core {
  // Specialize BuiltInObjectCreator for StructureClass_O
    template <>
    class BuiltInObjectCreator<StructureClass_O> : public core::Creator_O {
  public:
    typedef core::Creator_O TemplatedBase;
  public:
    DISABLE_NEW();
    size_t templatedSizeof() const { return sizeof(BuiltInObjectCreator<StructureClass_O>); };
    virtual void describe() const {
      printf("BuiltInObjectCreator for class %s  sizeof_instances-> %zu\n", _rep_(reg::lisp_classSymbol<StructureClass_O>()).c_str(), sizeof(StructureClass_O));
    }
    virtual core::T_sp creator_allocate() {
      // BuiltInObjectCreator<StructureClass_O> uses a different allocation method
      // that assigns the next Clos Stamp to the new StandardClass
      GC_ALLOCATE_VARIADIC(StructureClass_O, obj, gctools::NextStamp(),lisp_StandardClass(),REF_CLASS_NUMBER_OF_SLOTS_IN_STRUCTURE_CLASS );
      return obj;
    }
    virtual void searcher(){};
  };
};

#endif //]
