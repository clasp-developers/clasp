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

SMART(StringSet);

SMART(StructureClass);
class StructureClass_O : public Class_O {
  LISP_META_CLASS(StandardClass);
  LISP_BASE1(Class_O);
  LISP_CLASS(core, ClPkg, StructureClass_O, "structure-class");

public:
#if defined(XML_ARCHIVE)
  void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
  void initialize();
GCPROTECTED:
  Class_sp _InstanceCoreClass;

public:
  /*! Special creator used when starting up lisp environment, the object returned will be a root */
  static StructureClass_sp createUncollectable();
#if 0
	/*! Special creator used when starting up lisp environment */
	static StructureClass_sp create(Class_sp mc);
#endif

#if 0 // for now comment out all functions
	static StructureClass_sp create(Symbol_sp name /* , uint instanceClassSymbol */ );

	/*! ensure-class-using-class - see AOMOP-183 */
	static T_sp create_ensureClassUsingClass( Function_sp exec,
						  List_sp args,
						  Environment_sp env,
						  Lisp_sp lisp);

							      
        /*! Convert a Class list designator to a Cons of classes.
          Accept the following designators:
          nil - Convert to a Cons containing StructureClass.
          A single class - Convert to a Cons containing that Class.
          A cons of classes - return it.
        */
//    static List_sp classListDesignator(T_sp baseClassesDesignator, Lisp_sp lisp);


    public:
//	void defineYourSlotsFromBinderArchiveNode(ArchiveP node);
//	uint numberOfSlots();
        /*! Look for the symbol and return an iterator for the slot
         * otherwise return end()
         */
//	slotIterator find(Symbol_sp sym);
    public:

        /*! Reset the slots */
//	void resetSlots();

	void appendInstanceVariablesFromStructureClass(StructureClass_sp cc);
	void appendInstanceVariablesFromListOfSymbols(List_sp variableNames);

        virtual void describe(T_sp stream);
        virtual string dumpInfo();

#endif

  StructureClass_O();
  virtual ~StructureClass_O(){};
};
};

template <>
struct gctools::GCInfo<core::StructureClass_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

class StructureClassInitializationFunctoid : public Functoid {
private:
  StructureClass_sp _StructureClass;

public:
  DISABLE_NEW();

  virtual const char *describe() const { return "StructureClassInitializationFunctoid"; };
  StructureClassInitializationFunctoid(T_sp name, StructureClass_sp c) : Functoid(name) { this->_StructureClass = c; };
  virtual ~StructureClassInitializationFunctoid(){};
};
};
TRANSLATE(core::StructureClass_O);
#endif //]
