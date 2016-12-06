/*
    File: funcallableStandardClass.h
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
#ifndef _core_funcallableStandardClass_H
#define _core_funcallableStandardClass_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/stdClass.h>
#include <clasp/core/holder.h>

namespace core {
  SMART(FuncallableStandardClass);
};

template <>
struct gctools::GCInfo<core::FuncallableStandardClass_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
  SMART(FuncallableStandardClass);

  SMART(StringSet);

  SMART(FuncallableStandardClass);
  class FuncallableStandardClass_O : public StdClass_O {
    LISP_META_CLASS(core::StandardClass_O);
    LISP_CLASS(core, ClosPkg, FuncallableStandardClass_O, "FuncallableStandardClass",StdClass_O);

  public:
#if defined(XML_ARCHIVE)
    void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    void initialize();
  GCPROTECTED:
//    Class_sp _InstanceCoreClass;

  public:
  /*! Special creator used when starting up lisp environment */
    static FuncallableStandardClass_sp create(Class_sp mc);

#if 0 // for now comment out all functions
    static FuncallableStandardClass_sp create(Symbol_sp name /* , Symbol_sp instanceClassSymbol */ );

	/*! ensure-class-using-class - see AOMOP-183 */
    static T_sp create_ensureClassUsingClass( Function_sp exec,
                                              List_sp args,
                                              Environment_sp env,
                                              Lisp_sp lisp);
  public:
    void appendInstanceVariablesFromFuncallableStandardClass(FuncallableStandardClass_sp cc);
    void appendInstanceVariablesFromListOfSymbols(List_sp variableNames);
    virtual void describe(T_sp stream);
    virtual string dumpInfo();

#endif
    explicit FuncallableStandardClass_O(gctools::Stamp is) : StdClass_O(is) {};
  };
};

namespace core {
  // Specialize BuiltInObjectCreator for FuncallableStandardClass_O
    template <>
    class BuiltInObjectCreator<FuncallableStandardClass_O> : public core::Creator_O {
  public:
    typedef core::Creator_O TemplatedBase;
  public:
    DISABLE_NEW();
    size_t templatedSizeof() const { return sizeof(BuiltInObjectCreator<FuncallableStandardClass_O>); };
    virtual void describe() const {
      printf("BuiltInObjectCreator for class %s  sizeof_instances-> %zu\n", _rep_(reg::lisp_classSymbol<FuncallableStandardClass_O>()).c_str(), sizeof(FuncallableStandardClass_O));
    }
    virtual core::T_sp creator_allocate() {
      // BuiltInObjectCreator<FuncallableStandardClass_O> uses a different allocation method
      // that assigns the next Clos Stamp to the new StandardClass
      GC_ALLOCATE_VARIADIC(FuncallableStandardClass_O, obj, gctools::NextStamp() );
      return obj;
    }
    virtual void searcher(){};
  };


};




#endif //]
