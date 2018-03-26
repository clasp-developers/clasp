/*
    File: standardObject.cc
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
//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/standardObject.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/wrappers.h>

namespace core {

void StandardObject_O::initialize() {
  LOG(BF("Initializing StandardObject"));
  this->Base::initialize();
  //	Class_mv sc = cl__find_class(cl::_sym_StandardClass_O,true,_Nil<Environment_O>());
  //	this->_InstanceClass = sc.as<StandardClass_O>();
}

#if defined(OLD_SERIALIZE)
void StandardObject_O::serialize(serialize::SNode node) {
  IMPLEMENT_ME(); // handle slots properly so they are indexed by name
#if 0
	this->Base::serialize(node); 
	node->archiveObject("slots",this->_SlotBinder);
#endif
}
#endif

#if defined(XML_ARCHIVE)
void StandardObject_O::archiveBase(ArchiveP node) {
  IMPLEMENT_ME(); // handle slots properly so that they are indexed by name
#if 0
	this->Base::archiveBase(node); 
	node->archiveObject("slots",this->_SlotBinder);
#endif
}
#endif // defined(XML_ARCHIVE)

string StandardObject_O::__repr__() const {
  stringstream ss;
  ss << this->_instanceClass()->_classNameAsString() << " ";
#if 0
	ASSERT(this->_InstanceClass);
	if ( this->_InstanceClass.unboundp() )
	{
	    ss << "InstanceClass - UNBOUND -" << std::endl;
	} else
	{
	    ss << "InstanceClass: " << this->_InstanceClass->className() << std::endl;
	    ss << "[slots ";
	    ASSERT(this->_InstanceClass->_EffectiveSlotDefinitions);
	    if ( this->_InstanceClass->_EffectiveSlotDefinitions.unboundp() )
	    {
		ss << "_EffectiveSlotDefinitions are UNDEFINED";
	    } else
	    { 
		for ( Cons_sp cur = this->_InstanceClass->_EffectiveSlotDefinitions; cur.notnilp(); cur=cCdr(cur) )
		{
		    EffectiveSlotDefinition_sp slotDef = cur->ocar().as<EffectiveSlotDefinition_O>();
		    ss << slotDef->_SlotName->__repr__();
		}
	    }
	    ss << "]" << std::endl;
	}
#endif
  return ss.str();
}

#if 0
    T_sp& StandardObject_O::slot_ref(Symbol_sp slot_name) throw(SlotRefFailed)
    {_OF();
	IMPLEMENT_ME();
	StandardClass_sp cc = this->__class().as<StandardClass_O>();
	ASSERT(cc.notnilp());
	// TODO: What about class slots? slot_location returns the index of an instance slot
	// but where do we store class slots and how do we return a reference to one of them
	LOG(BF("This object is of class: %s") % cc->__repr__() );
	uint location = cc->slot_location(slot_name); // Can throw SlotRefFailed if slot_name not found
	LOG(BF("Found the slot with name: %s")% slot_name->__repr__() );
	LOG(BF("   the number of slots in this object are: %d")% this->_Slots.size());
	return this->_Slots[location];
    }
#endif

#if 0
    void StandardObject_O::allocate_slot_storage(uint numSlots, T_sp initialValue )
    {
	this->_Slots.resize(numSlots,initialValue);
    }
#endif





};
