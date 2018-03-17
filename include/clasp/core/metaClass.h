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
#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunneeded-internal-declaration"
//#pragma clang diagnostic ignored "-Wunused-local-typedef"
#include <boost/iterator_adaptors.hpp>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>
#pragma clang diagnostic pop
#include <clasp/core/array.h>
#include <clasp/core/object.fwd.h>
#include <clasp/core/numbers.fwd.h>
//#include <clasp/core/holder.h>
#include <clasp/core/instance.h>


namespace core {

  typedef Instance_O Class_O;
  typedef Instance_sp Class_sp;

  Fixnum_sp clasp_make_fixnum(gc::Fixnum v);
  
//
//  Class access functions for when we only have a forward
//  definition for the Class_O class
//

/*! Class spoofs ECL>>Instance_O classes by doing the following.

  virtual T_sp Class_O::instanceClass() is overloaded in StandardClass_O and BuiltInClass_O
  and they each return XXXX::___staticClass.
  This should never change because you should never change the meta classes of instances of StandardClass and BuiltInClass

  virtual T_sp Class_O::instanceSig() is overloaded in StandardClass_O and BuiltInClass_O
  and they each return XXXXClass::___staticClass->slots()
  This should never change because you should never change the class slots of StandardClass_O or BuiltInClass_O

  
*/

#define REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS 24
#define REF_CLASS_NUMBER_OF_SLOTS_IN_STRUCTURE_CLASS 31
#define REF_CLASS_NUMBER_OF_SLOTS_IN_DERIVABLE_CXX_CLASS REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS
  
  // Specialize BuiltInObjectCreator for DummyStandardClass_O
  template <>
    class BuiltInObjectCreator<Class_O> : public core::Creator_O {
  public:
    typedef core::Creator_O TemplatedBase;
  public:
  public:
    size_t templatedSizeof() const { return sizeof(BuiltInObjectCreator<Class_O>); };
    bool creates_classes() const { return true; };
    virtual core::T_sp creator_allocate() {
      // BuiltInObjectCreator<Class_O> uses a different allocation method
      // that assigns the next Clos Stamp to the new Class
      GC_ALLOCATE_VARIADIC(Class_O, obj, lisp_standard_class() /*,REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS */);
//      printf("%s:%d  creating class\n", __FILE__, __LINE__ );
      return obj;
    }
    virtual void searcher(){};
  };
};

namespace core {

/*!Return true if low is a subclass of high */
bool core__subclassp(T_sp low, T_sp high);

/*! Return true if the object is of the class _class */
bool af_ofClassP(T_sp object, T_sp _class);


};
#endif
