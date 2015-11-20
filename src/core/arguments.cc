/*
    File: arguments.cc
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
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/arguments.h>

namespace core {

List_sp Argument::lambdaList() const {
  return ((this->_ArgTarget));
};

Symbol_sp Argument::symbol() const {
  return ((gc::As<Symbol_sp>(this->_ArgTarget)));
};

List_sp Argument::classified() const {
  _G();
  if (this->_ArgTargetFrameIndex == SPECIAL_TARGET) {
    return coerce_to_list(Cons_O::create(ext::_sym_specialVar, this->_ArgTarget));
  } else if (this->_ArgTargetFrameIndex >= 0) {
    return coerce_to_list(Cons_O::create(ext::_sym_lexicalVar, Cons_O::create(this->_ArgTarget, make_fixnum(this->_ArgTargetFrameIndex))));
  } else if (this->_ArgTargetFrameIndex == UNDEFINED_TARGET) {
    return ((_Nil<List_V>()));
  }
  SIMPLE_ERROR(BF("Illegal target"));
}

LambdaListHandler_sp Argument::lambdaListHandler() const {
  return ((gc::As<LambdaListHandler_sp>(this->_ArgTarget)));
}

string Argument::asString() const {
  stringstream ss;
  ss << "#<Argument ";
  ss << ":target ";
  ss << _rep_(this->_ArgTarget);
  ss << " :tfi ";
  ss << this->_ArgTargetFrameIndex;
  ss << " >  ";
  return ((ss.str()));
}

string ArgumentWithDefault::asString() const {
  stringstream ss;
  ss << "#<ArgumentWithDefault ";
  ss << ":target ";
  ss << _rep_(this->_ArgTarget);
  ss << " :tfi ";
  ss << this->_ArgTargetFrameIndex;
  ss << " :default ";
  ss << _rep_(this->_Default);
  ss << " >  ";
  return ((ss.str()));
}

string RequiredArgument::asString() const {
  stringstream ss;
  ss << "#<RequiredArgument ";
  ss << ":target ";
  this->Base::asString();
  ss << " >  ";
  return ((ss.str()));
}

string OptionalArgument::asString() const {
  stringstream ss;
  ss << "#<OptionalArgument ";
  ss << ":target ";
  ss << this->Base::asString();
  if (this->_Sensor.isDefined()) {
    ss << " :sensor ";
    ss << this->_Sensor.asString();
  }
  ss << " >  ";
  return ((ss.str()));
}

string RestArgument::asString() const {
  stringstream ss;
  ss << "#<RestArgument ";
  ss << ":target ";
  ss << _rep_(this->_ArgTarget);
  ss << " :tfi ";
  ss << this->_ArgTargetFrameIndex;
  ss << " >  ";
  return ((ss.str()));
}

string KeywordArgument::asString() const {
  stringstream ss;
  ss << "#<KeywordArgument ";
  ss << ":keyword " << _rep_(this->_Keyword);
  ss << " :target ";
  ss << this->Base::asString();
  if (this->_Sensor.isDefined()) {
    ss << " :sensor ";
    ss << this->_Sensor.asString();
  }
  ss << " >  ";
  return ((ss.str()));
}

string AuxArgument::asString() const {
  stringstream ss;
  ss << "#<AuxArgument ";
  ss << ":target ";
  this->Base::asString();
  ss << " :expression ";
  ss << _rep_(this->_Expression);
  ss << " >  ";
  return ((ss.str()));
}

#if 0 // moved to header
DynamicScopeManager::DynamicScopeManager() {
  int top = _lisp->bindings().top();
  this->_beginTop = top;
  this->_endTop = top;
}

DynamicScopeManager::DynamicScopeManager(Symbol_sp sym, T_sp val) {
  int top = _lisp->bindings().top();
  this->_beginTop = top;
  this->_endTop = top;
  this->pushSpecialVariableAndSet(sym, val);
}
#endif

void DynamicScopeManager::dump() const {
  stringstream ss;
  ss << "DynamicScopeManager  _beginTop[" << this->_beginTop << "] _endTop[" << this->_endTop << "]" << std::endl;
  for (int i = this->_endTop - 1; i >= this->_beginTop; --i) {
    ss << (BF("Dynamic[%d] var[%s] val-->%s") % i % _rep_(_lisp->bindings().var(i)) % _rep_(_lisp->bindings().val(i))).str() << std::endl;
  }
  printf("%s", ss.str().c_str());
}

void DynamicScopeManager::pushSpecialVariableAndSet(Symbol_sp sym, T_sp val) {
  _lisp->bindings().push(sym);
  this->_endTop = _lisp->bindings().top();
  //	SymbolSaveValue sv(sym,sym->symbolValueUnsafe());
  //	this->_SavedValues.push_back(sv);
  sym->setf_symbolValue(val);
}

/*! The frame_index is not used here - it is only used by ActivationFrameDynamicLexicalScopeManager */
void DynamicScopeManager::new_binding(const Argument &arg, T_sp val) {
  if (arg._ArgTargetFrameIndex == SPECIAL_TARGET) {
    Symbol_sp sym = gc::As<Symbol_sp>(arg._ArgTarget);
    this->pushSpecialVariableAndSet(sym, val);
    return;
  }
  SIMPLE_ERROR(BF("DynamicScopeManager doesn't bind anything other than SPECIAL_TARGET bindings - you gave it a binding to[%s] index[%d]") % _rep_(arg._ArgTarget) % arg._ArgTargetFrameIndex);
}

T_sp DynamicScopeManager::lexenv() const {
  SIMPLE_ERROR(BF("A ValueEnvironment was requested from a DynamicScopeManager - only ValueEnvironmentDynamicScopeManagers have those"));
}

#if 0
DynamicScopeManager::~DynamicScopeManager() {
  DynamicBindingStack &bindings = _lisp->bindings();
  int numBindings = this->_endTop - this->_beginTop;
  for (int i = 0; i < numBindings; ++i) {
    bindings.pop();
  }
}
#endif

bool ValueEnvironmentDynamicScopeManager::lexicalElementBoundP(const Argument &argument) {
  return ((this->_Environment->activationFrameElementBoundP(argument._ArgTargetFrameIndex)));
}

void ValueEnvironmentDynamicScopeManager::new_binding(const Argument &argument, T_sp val) {
  if (argument._ArgTargetFrameIndex == SPECIAL_TARGET) {
    this->DynamicScopeManager::new_binding(argument, val);
    return;
  }
  ASSERTF(argument._ArgTargetFrameIndex >= 0, BF("Illegal ArgTargetIndex[%d] for lexical variable[%s]") % argument._ArgTargetFrameIndex % _rep_(argument._ArgTarget));
  T_sp argTarget = argument._ArgTarget;
  this->_Environment->new_binding(gc::As<Symbol_sp>(argTarget), argument._ArgTargetFrameIndex, val);
}

void ValueEnvironmentDynamicScopeManager::new_variable(List_sp classified, T_sp val) {
  Symbol_sp type = gc::As<Symbol_sp>(oCar(classified));
  if (type == ext::_sym_specialVar) {
    Symbol_sp sym = gc::As<Symbol_sp>(oCdr(classified));
    this->DynamicScopeManager::pushSpecialVariableAndSet(sym, val);
    return;
  } else if (type == ext::_sym_lexicalVar) {
    Symbol_sp sym = gc::As<Symbol_sp>(oCadr(classified));
    int idx = unbox_fixnum(gc::As<Fixnum_sp>(oCddr(classified)));
    ASSERTF(idx >= 0, BF("Illegal target index[%d] for lexical variable[%s]") % idx % _rep_(sym));
    this->_Environment->new_binding(sym, idx, val);
    return;
  }
  SIMPLE_ERROR(BF("Illegal classified type: %s\n") % _rep_(classified));
}

void ValueEnvironmentDynamicScopeManager::new_special(List_sp classified) {
  ASSERT(oCar(classified) == _sym_declaredSpecial);
  Symbol_sp sym = gc::As<Symbol_sp>(oCdr(classified));
  this->_Environment->defineSpecialBinding(sym);
}

void ActivationFrameDynamicScopeManager::new_binding(const Argument &argument, T_sp val) {
  if (argument._ArgTargetFrameIndex == SPECIAL_TARGET) {
    this->DynamicScopeManager::new_binding(argument, val);
    return;
  }
  ASSERTF(argument._ArgTargetFrameIndex >= 0, BF("Illegal ArgTargetIndex[%d] for lexical variable[%s]") % argument._ArgTargetFrameIndex % _rep_(argument._ArgTarget));
  this->_Frame->set_entry(argument._ArgTargetFrameIndex, val);
}

bool ActivationFrameDynamicScopeManager::lexicalElementBoundP(const Argument &argument) {
  return ((this->_Frame->boundp_entry(argument._ArgTargetFrameIndex)));
}

T_sp ActivationFrameDynamicScopeManager::lexenv() const {
  //	SIMPLE_ERROR(BF("A ValueEnvironment was requested from a DynamicScopeManager... \n but only ValueEnvironmentDynamicScopeManagers have those.   \n On the other hand, ActivationFrameDynamicScopeManager::lexenv() \n should only be called when evaluating lambda-list init-forms \n (&optional,&key,&aux) and those should be evaluated in an environment \n that only has the lambda-list bindings in the top-level-environment \n - Since we attach the binding symbol names to the ActivationFrame we \n could just use the ActivationFrame of this ActivationFrameDynamicScopeManager \n as the environment that lexenv returns"));
  // I'm going to return the ActivationFrame here and ASSERT that it must have debugging info
  // attached.  I don't think the caller should be evaluating expressions in the environment
  // represented by this->_Frame unless it has symbol names attached to it.
  // I'm not sure the ActivationFrames with debugging information honor all of the
  // variable/function lookup and update functions though so even with debugging information
  // providing symbol names of variables it may not work - meister Nov 2013
  return this->_Frame;
}

void StackFrameDynamicScopeManager::new_binding(const Argument &argument, T_sp val) {
  if (argument._ArgTargetFrameIndex == SPECIAL_TARGET) {
    this->DynamicScopeManager::new_binding(argument, val);
    return;
  }
  ASSERTF(argument._ArgTargetFrameIndex >= 0, BF("Illegal ArgTargetIndex[%d] for lexical variable[%s]") % argument._ArgTargetFrameIndex % _rep_(argument._ArgTarget));
  this->frame[argument._ArgTargetFrameIndex] = val.raw_();
}

void StackFrameDynamicScopeManager::va_rest_binding(const Argument &argument) {
  if (argument._ArgTargetFrameIndex == SPECIAL_TARGET) {
    SIMPLE_ERROR(BF("You cannot bind &VA-REST argument to a special"));
  }
  ASSERTF(argument._ArgTargetFrameIndex >= 0, BF("Illegal ArgTargetIndex[%d] for lexical variable[%s]") % argument._ArgTargetFrameIndex % _rep_(argument._ArgTarget));
  VaList_sp valist(&this->valist());
  this->frame[argument._ArgTargetFrameIndex] = valist.raw_();
}

bool StackFrameDynamicScopeManager::lexicalElementBoundP(const Argument &argument) {
  //  core::T_O **array(frame::ValuesArray(this->frame));
  return !gctools::tagged_unboundp(this->frame[argument._ArgTargetFrameIndex]);
}

T_sp StackFrameDynamicScopeManager::lexenv() const {
  //  printf("%s:%d Returning nil as the lexical environment for a StackFrameDynamicScopeManager\n", __FILE__, __LINE__);
  return _Nil<core::T_O>();
}
#if 0 // Oh oh - do I need these?
T_sp StackFrameDynamicScopeManager::activationFrame() const {
  return this->frame;
}
#endif
};
