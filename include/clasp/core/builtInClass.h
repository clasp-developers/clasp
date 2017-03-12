/*
    File: builtInClass.h
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
#ifndef _core_BuiltInClass_O_H //[
#define _core_BuiltInClass_O_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/holder.h>

#if 0
namespace core {

SMART(BuiltInClass);
class BuiltInClass_O : public Class_O {
  LISP_META_CLASS(core::StandardClass_O);
  LISP_CLASS(core, ClPkg, BuiltInClass_O, "BuiltInClass",Class_O);

private:
  //	string			_InitializationArgumentString;
  //	LambdaListHandler_sp	_InitializationArguments;
public:
#if defined(XML_ARCHIVE)
  void archive(ArchiveP node);
#endif // defined(XML_ARCHIVE)
  void initialize();

public:
  /*! The normal BuiltInClass creator used once the Lisp environment has been bootstrapped */
  static BuiltInClass_sp create(Symbol_sp instanceClassSymbol);
  /*! Create a BuiltInClass_sp that will always be considered a root object */
  static BuiltInClass_sp createUncollectable(gctools::Stamp is);

public:
  virtual void describe(T_sp stream);

  virtual bool builtInClassP() const { return true; }

  /*! Allocate and initialize an instance of this class
         */
  T_sp allocateAndInitialize();
  explicit BuiltInClass_O(gctools::Stamp is) : Class_O(is) {};
  virtual ~BuiltInClass_O(){};
};
};
template <>
struct gctools::GCInfo<core::BuiltInClass_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
  // Specialize BuiltInObjectCreator for BuiltInClass_O
  template <>
    class BuiltInObjectCreator<BuiltInClass_O> : public core::Creator_O {
  public:
    typedef core::Creator_O TemplatedBase;
  public:
    DISABLE_NEW();
    size_t templatedSizeof() const { return sizeof(BuiltInObjectCreator<BuiltInClass_O>); };
    virtual void describe() const {
      printf("BuiltInObjectCreator for class %s  sizeof_instances-> %zu\n", _rep_(reg::lisp_classSymbol<BuiltInClass_O>()).c_str(), sizeof(BuiltInClass_O));
    }
    virtual core::T_sp creator_allocate() {
      // BuiltInObjectCreator<BuiltInClass_O> uses a different allocation method
      // that assigns the NextStamp to the new BuiltInClass
      GC_ALLOCATE_VARIADIC(BuiltInClass_O, obj, gctools::NextStamp() );
      return obj;
    }
    virtual void searcher(){};
  };


};
#endif //]
#endif
