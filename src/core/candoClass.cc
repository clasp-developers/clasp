/*
    File: candoClass.cc
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
#include <candoClass.h>
#include <clasp/core/lisp.h>
#include <effectiveSlotDefinition.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/package.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/wrappers.h>

namespace core {

void CandoClass_O::initialize() {
  this->Base::initialize();
  this->_CoreObjectClass = _Nil<Class_O>();
}

CandoClass_sp CandoClass_O::create(Lisp_sp lisp, Symbol_sp name) {
  GC_ALLOCATE(CandoClass_O);
  oclass->_Name = name;
  // Lets create a predefined symbol here for the class name
  oclass->_InstanceClassSymbol = UNDEFINED_SYMBOL;
  _lisp->createPredefinedSymbol(oclass->_InstanceClassSymbol, name);
  //    oclass->_InstanceVariableNames = _Nil<T_O>();
  IMPLEMENT_MEF("What do I do about superclasses of the CandoClass here? - I should call setInstanceBaseClasses with something or just StandardObject ");
  return oclass;
}

#if defined(XML_ARCHIVE)
void CandoClass_O::archiveBase(ArchiveP node) {
  _OF();
  IMPLEMENT_MEF("Implement CandoClass_O::archiveBase");
}
#endif // defined(XML_ARCHIVE)

Class_sp CandoClass_O::getCoreObjectClass() {
  return this->_CoreObjectClass;
}

void CandoClass_O::setCoreObjectClass(Class_sp mc) {
  this->_CoreObjectClass = mc;
}

void CandoClass_O::describe() {
  _lisp->print(BF("------------  CandoClass name: %s    instanceClassSymbol: %d") % this->_Name->__repr__() % this->_InstanceClassSymbol);
  //    _lisp->print(BF("Instance variables: %s") % this->_InstanceVariableNames->__repr__().c_str() );
  _lisp->print(BF("%s") % this->dumpInfo());
}

string CandoClass_O::dumpInfo() {
  stringstream ss;
  ss << this->Base::dumpInfo();
  ss << "CoreObjectClass: " << this->_CoreObjectClass->getPackagedName() << std::endl;
  return ss.str();
}

string CandoClass_O::dumpMethods() {
  return this->Base::dumpMethods();
}

Cons_sp CandoClass_O::find(Symbol_sp sym) {
  IMPLEMENT_MEF("Handle find slot by name");
#if 0
	ASSERTNOTNULL(sym);
	LOG(BF("Looking in CandoClass for slot for symbol: %s") % sym->fullName() );
	CandoClass_O::slotIterator fnd = this->_SlotNames.find(sym);
	return this->_SlotNames.find(sym);
#endif
}

void CandoClass_O::setupAccessors(List_sp slotNames) {
  IMPLEMENT_ME(); // Dont pass the slot names, use the slots already defined
#if 0
	this->_InstanceVariableNames = slotNames;
	while ( slotNames.notnilp() )
	{
	    Symbol_sp slotName = slotNames->ocar().as<Symbol_O>();
	    string setterName = "set_"+slotName->symbolNameAsString();
	    Symbol_sp setterSymbol = _lisp->internKeyword(setterName);
	    SlotSetter_sp setterForm = SlotSetter_O::create(setterSymbol,_lisp);
	    this->addMethod(setterSymbol,setterForm);
	    string getterName = "get_"+slotName->symbolNameAsString();
	    Symbol_sp getterSymbol = _lisp->internKeyword(getterName);
	    SlotGetter_sp getterForm = SlotGetter_O::create(getterSymbol,_lisp);
	    this->addMethod(getterSymbol,getterForm);
	    slotNames = slotNames->cdr();
	}
#endif
}




};
