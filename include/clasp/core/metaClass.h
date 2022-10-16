/*
    File: metaClass.h
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

#ifndef Class_H
#define Class_H

#include <list>
#include <clasp/core/array.h>
#include <clasp/core/object.fwd.h>
#include <clasp/core/numbers.fwd.h>
#include <clasp/core/instance.h>


namespace core {

  Fixnum_sp clasp_make_fixnum(gc::Fixnum v);
  

/*! Class spoofs ECL>>Instance_O classes by doing the following.

  virtual T_sp Instance_O::instanceClass() is overloaded in StandardInstance_O and BuiltInClass_O
  and they each return XXXX::___staticClass.
  This should never change because you should never change the meta classes of instances of StandardClass and BuiltInClass

  virtual T_sp Instance_O::instanceSig() is overloaded in StandardInstance_O and BuiltInClass_O
  and they each return XXXXClass::___staticClass->slots()
  This should never change because you should never change the class slots of StandardInstance_O or BuiltInClass_O

  
*/

#define REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS 21
#define REF_CLASS_NUMBER_OF_SLOTS_IN_STRUCTURE_CLASS 24
#define REF_CLASS_NUMBER_OF_SLOTS_IN_DERIVABLE_CXX_CLASS REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS
  
  // Specialize WRAPPER_BuiltInObjectCreator for DummyStandardInstance_O
  template <>
    class WRAPPER_BuiltInObjectCreator<Instance_O> : public core::Creator_O {
  public:
    typedef core::Creator_O TemplatedBase;
  public:
  public:
    size_t templatedSizeof() const { return sizeof(WRAPPER_BuiltInObjectCreator<Instance_O>); };
    bool creates_classes() const { return true; };
    virtual core::T_sp creator_allocate() {
      // WRAPPER_BuiltInObjectCreator<Instance_O> uses a different allocation method
      // that assigns the next Clos Stamp to the new Class
      auto  obj = gctools::GC<Instance_O>::allocate( lisp_standard_class() /*,REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS */);
//      printf("%s:%d  creating class\n", __FILE__, __LINE__ );
      return obj;
    }
    virtual void searcher(){};
    WRAPPER_BuiltInObjectCreator<Instance_O>(GlobalSimpleFun_sp fdesc) : Creator_O(fdesc) {};
  };
};

namespace core {

/*!Return true if low is a subclass of high */
bool core__subclassp(T_sp low, T_sp high);

};
#endif

