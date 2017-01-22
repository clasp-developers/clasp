/*
    File: standardClass.h
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
#ifndef _core_standardClass_H
#define _core_standardClass_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/stdClass.h>
#include <clasp/core/holder.h>

template <>
struct gctools::GCInfo<core::StandardClass_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

SMART(StandardClass);
SMART(StringSet);
SMART(StandardClass);
class StandardClass_O : public StdClass_O {
  LISP_META_CLASS(core::StandardClass_O);
  LISP_CLASS(core, ClPkg, StandardClass_O, "StandardClass",StdClass_O);

public:
#if defined(XML_ARCHIVE)
  void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
  void initialize();
GCPROTECTED:
//  Class_sp _InstanceCoreClass;

public:
  /*! Special creator used when starting up lisp environment */
  //	static StandardClass_sp create(Class_sp mc);

  /*! Special creator used when bootstrapping - the resulting will never be collected
          and will always be treated as a root */
  static StandardClass_sp createUncollectable(gctools::Stamp is);

 public:
  void reinitialize_standard_class_stamp() {
    printf("%s:%d Why am I calculating the stamp twice? see source below\n", __FILE__, __LINE__);
    this->_instance_stamp = gctools::NextStamp();
  };
  explicit StandardClass_O(gctools::Stamp is) : StdClass_O(is) {};
  virtual ~StandardClass_O(){};

  virtual void reinitialize_class() {
//    printf("%s:%d Why am I calculating the stamp twice? see source above\n", __FILE__, __LINE__);
    this->_instance_stamp = gctools::NextStamp(); };

};
};


namespace core {
  // Specialize BuiltInObjectCreator for StandardClass_O
    template <>
    class BuiltInObjectCreator<StandardClass_O> : public core::Creator_O {
  public:
    typedef core::Creator_O TemplatedBase;
  public:
    DISABLE_NEW();
  public:
    size_t templatedSizeof() const { return sizeof(BuiltInObjectCreator<StandardClass_O>); };
    virtual void describe() const {
      printf("BuiltInObjectCreator for class %s  sizeof_instances-> %zu\n", _rep_(reg::lisp_classSymbol<StandardClass_O>()).c_str(), sizeof(StandardClass_O));
    }
    virtual core::T_sp creator_allocate() {
      // BuiltInObjectCreator<StandardClass_O> uses a different allocation method
      // that assigns the next Clos Stamp to the new StandardClass
      GC_ALLOCATE_VARIADIC(StandardClass_O, obj, gctools::NextStamp() );
      return obj;
    }
    virtual void searcher(){};
  };


};


#endif //]
