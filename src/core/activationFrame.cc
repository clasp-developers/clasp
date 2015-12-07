/*
    File: activationFrame.cc
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
#include <clasp/core/activationFrame.h>
#include <clasp/core/lispVector.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/wrappers.h>

namespace core {

void watchTriggered(T_sp *ptr) {
  printf("%s:%d Watch-pointer@%p triggered on:", __FILE__, __LINE__, ptr);
  if ((*ptr)) {
    printf("%s\n", _rep_((*ptr)).c_str());
  } else {
    printf(" UNDEFINED!!!!\n");
  }
}

T_sp ActivationFrame_O::currentVisibleEnvironment() const {
  _G();
  return this->const_sharedThis<ActivationFrame_O>();
}

T_sp ActivationFrame_O::getActivationFrame() const {
  return this->const_sharedThis<ActivationFrame_O>();
}

string ActivationFrame_O::clasp_asString(T_sp af) {
  if (af.nilp()) {
    stringstream ss;
    ss << "#<" << af->_instanceClass()->classNameAsString() << " NIL>";
    return ((ss.str()));
  }
  return gc::As<ActivationFrame_sp>(af)->asString();
}

string ActivationFrame_O::asString() const {
  SUBIMP();
}

bool ActivationFrame_O::_findTag(Symbol_sp sym, int &depth, int &index, bool &interFunction, T_sp &tagbodyEnv) const {
  _G();
  T_sp parent = clasp_currentVisibleEnvironment(this->getParentEnvironment());
  ++depth;
  return clasp_findTag(parent, sym, depth, index, interFunction, tagbodyEnv);
}

bool ActivationFrame_O::_findValue(T_sp sym, int &depth, int &index, ValueKind &valueKind, T_sp &value) const {
  _G();
  T_sp parent = clasp_currentVisibleEnvironment(this->getParentEnvironment());
  ++depth;
  return clasp_findValue(parent, sym, depth, index, valueKind, value);
}

bool ActivationFrame_O::_findFunction(T_sp functionName, int &depth, int &index, Function_sp &func) const {
  T_sp parent = clasp_currentVisibleEnvironment(this->getParentEnvironment());
  ++depth;
  return clasp_findFunction(parent, functionName, depth, index, func);
}

string ActivationFrame_O::summaryOfContents() const {
  SUBCLASS_MUST_IMPLEMENT();
}

T_sp ActivationFrame_O::_lookupTagbodyId(int depth, int index) const {
  _G();
  if (depth == 0) {
    SIMPLE_ERROR(BF("Hit depth=0 and did not find value - this activation frame: %s") % this->__repr__());
  }
  --depth;
  return Environment_O::clasp_lookupTagbodyId(this->parentFrame(), depth, index);
}

T_sp &ActivationFrame_O::lookupValueReference(int depth, int index) {
  _G();
  if (depth == 0) {
    SIMPLE_ERROR(BF("Hit depth=0 and did not find value - this activation frame: %s") % this->__repr__());
  }
  --depth;
  return Environment_O::clasp_lookupValueReference(this->parentFrame(), depth, index);
}

T_sp ActivationFrame_O::_lookupValue(int depth, int index) {
  _G();
  return this->lookupValueReference(depth, index);
}

Function_sp ActivationFrame_O::_lookupFunction(int depth, int index) const {
  _G();
  if (depth == 0) {
    SIMPLE_ERROR(BF("Hit depth=0 and did not find function - this activation frame: %s") % this->__repr__());
  }
  --depth;
  return Environment_O::clasp_lookupFunction(this->parentFrame(), depth, index);
}

EXPOSE_CLASS(core, ActivationFrame_O);

void ActivationFrame_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<ActivationFrame_O>();
}

void ActivationFrame_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, ActivationFrame, "", "", _lisp);
#endif
}
};

namespace core {

string ValueFrame_O::summaryOfContents() const {
  stringstream ss;
  ss << "---" << this->_instanceClass()->classNameAsString() << "#" << this->environmentId() << " :len " << this->length() << std::endl;
  T_sp debuggingInfo = _Nil<T_O>();
  if (this->_DebuggingInfo.notnilp()) {
    debuggingInfo = gc::As<Vector_sp>(this->_DebuggingInfo);
  }
  for (int i = 0; i < this->_Objects.capacity(); ++i) {
    if (debuggingInfo.notnilp() && (i < cl__length(gc::As<Vector_sp>(debuggingInfo)))) {
      ss << _rep_(gc::As<Vector_sp>(debuggingInfo)->elt(i)) << " ";
    } else {
      ss << ":arg" << i << "@" << (void *)(&(this->operator[](i))) << " ";
    }
    if (!this->operator[](i)) {
      ss << "UNDEFINED";
    } else if (!this->boundp_entry(i)) {
      ss << "!!UNBOUND!! ";
    } else {
      if (core__activation_frame_p(this->operator[](i))) {
        ss << "ActivationFrame@" << (void *)(&(this->operator[](i)));
      } else {
        ss << "-->" << _rep_(this->operator[](i)) << "  ";
      }
    }
    ss << std::endl;
  }
  return ((ss.str()));
}

string ValueFrame_O::asString() const {
  return this->summaryOfContents();
}

T_sp &ValueFrame_O::operator[](int index) {
  _G();
  ASSERTF(index < this->_Objects.capacity(), BF("Out of range index %d for ValueFrame with %d entries") % index % this->_Objects.capacity());
  return ((this->_Objects[index]));
}

const T_sp &ValueFrame_O::operator[](int index) const {
  _G();
  ASSERTF(index < this->_Objects.capacity(), BF("Out of range index %d for ValueFrame with %d entries") % index % this->_Objects.capacity());
  return ((this->_Objects[index]));
}

T_sp &ValueFrame_O::lookupValueReference(int depth, int index) {
  _G();
  if (depth == 0) {
    ASSERTF(index < this->_Objects.capacity(), BF("Out of range index %d for ValueFrame with %d entries") % index % this->_Objects.capacity());
    return ((this->_Objects[index]));
  }
  --depth;
  return Environment_O::clasp_lookupValueReference(this->parentFrame(), depth, index);
}

void ValueFrame_O::fillRestOfEntries(int istart, List_sp values) {
  _G();
  ASSERTF((istart + cl__length(values)) == this->length(), BF("Mismatch between size of ValueFrame[%d] and the number of entries[%d] that are about to fill it") % this->length() % (istart + cl__length(values)));
  int iend = this->length();
  ASSERT(values.consp());
  List_sp cur = values;
  for (int i = istart; i < iend; ++i) {
    ASSERT(oCar(cur));
    this->set_entry(i, oCar(cur));
    cur = oCdr(cur);
  }
}

T_sp ValueFrame_O::_lookupValue(int depth, int index) {
  _G();
  return this->lookupValueReference(depth, index);
}

ValueFrame_sp ValueFrame_O::createForLambdaListHandler(LambdaListHandler_sp llh, T_sp parent) {
  _G();
  ValueFrame_sp vf(ValueFrame_O::create(llh->numberOfLexicalVariables(), parent));
  return ((vf));
}

ValueFrame_sp ValueFrame_O::create(List_sp values, T_sp parent) {
  _G();
  ValueFrame_sp vf = ValueFrame_O::create(cl__length(values), parent);
  //	vf->allocateStorage(cl__length(values));
  int idx = 0;
  for (auto cur : values) {
    vf->_Objects[idx] = oCar(cur);
    ++idx;
  }
  return ((vf));
}

ValueFrame_sp ValueFrame_O::createFromReversedCons(List_sp values, T_sp parent) {
  _G();
  ValueFrame_sp vf = ValueFrame_O::create(cl__length(values), parent);
  int len = cl__length(values);
  //	vf->allocateStorage(len);
  int idx = len - 1;
  for (auto cur : values) {
    T_sp val = oCar(cur);
    ASSERTNOTNULL(val);
    vf->_Objects[idx] = val;
    --idx;
  }
  return ((vf));
}

/*! Update a value in the frame based on it's name
     This is only used by the interpreter and so it isn't expected to be fast
    */
bool ValueFrame_O::_updateValue(Symbol_sp sym, T_sp obj) {
  if (this->_DebuggingInfo.nilp()) {
    return this->Base::_updateValue(sym, obj);
  }
  Vector_sp debuggingInfo = gc::As<Vector_sp>(this->_DebuggingInfo);
  for (int i(0), iEnd(this->length()); i < iEnd; ++i) {
    if (gc::As<Symbol_sp>(debuggingInfo->elt(i)) == sym) {
      this->_Objects[i] = obj;
      return true;
    }
  }
  if (this->parentFrame().nilp())
    return false;
  return af_updateValue(this->parentFrame(), sym, obj);
}

/*! Find the value bound to a symbol based on the symbol name.
       This is only used by the interpreter and shouldn't be expected to be fast.
    */
bool ValueFrame_O::_findValue(T_sp sym, int &depth, int &index, ValueKind &valueKind, T_sp &value) const {
  _G();
  //	printf("%s:%d ValueFrame_O::_findValue - switch to DWARF debugging to look up values\n", __FILE__, __LINE__ );
  if (this->_DebuggingInfo.nilp()) {
    return ((this->Base::_findValue(sym, depth, index, valueKind, value)));
  }
  if (!this->_DebuggingInfo) {
    THROW_HARD_ERROR(BF("The debugging info was NULL!!!!! Why is this happening?"));
  }
  Vector_sp debuggingInfo = gc::As<Vector_sp>(this->_DebuggingInfo);
  int i = 0;
  for (; i < this->length(); ++i) {
    if (gc::As<Symbol_sp>(debuggingInfo->elt(i)) == sym) {
      index = i;
      value = this->_Objects[i];
      valueKind = lexicalValue;
      return true;
    }
  }
  ++depth;
  return Environment_O::clasp_findValue(this->parentFrame(), sym, depth, index, valueKind, value);
}

EXPOSE_CLASS(core, ValueFrame_O);

void ValueFrame_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<ValueFrame_O>();
}

void ValueFrame_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, ValueFrame, "", "", _lisp);
#endif
}

string FunctionFrame_O::summaryOfContents() const {
  return (this->asString());
}

string FunctionFrame_O::asString() const {
  stringstream ss;
  ss << "#<[" << this->_instanceClass()->classNameAsString() << " :len " << this->length() << " ";
  for (int i = 0; i < this->_Objects.capacity(); ++i) {
    ss << _rep_(this->_Objects[i]) << " " << std::endl;
  }
  ss << "]>";
  return ((ss.str()));
}

Function_sp FunctionFrame_O::_lookupFunction(int depth, int index) const {
  _G();
  if (depth == 0) {
    if (index >= this->_Objects.capacity()) {
      SIMPLE_ERROR(BF("Out of range index[%d] for FunctionFrame with %d entries") % index % this->_Objects.capacity());
    }
    return gc::As<Function_sp>((this->entry(index)));
  }
  --depth;
  return Environment_O::clasp_lookupFunction(this->parentFrame(), depth, index);
}

T_sp FunctionFrame_O::entry(int idx) const {
  _G();
  ASSERTF(idx < this->_Objects.capacity(), BF("index[%d] out of range for writing to activation frame with %d slots") % idx % this->_Objects.capacity());
  return (this->_Objects[idx]);
}

const T_sp &FunctionFrame_O::entryReference(int idx) const {
  _G();
  ASSERTF(idx < this->_Objects.capacity(), BF("index[%d] out of range for writing to activation frame with %d slots") % idx % this->_Objects.capacity());
  return (this->_Objects[idx]);
}

EXPOSE_CLASS(core, FunctionFrame_O);

void FunctionFrame_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<FunctionFrame_O>();
}

void FunctionFrame_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, FunctionFrame, "", "", _lisp);
#endif
}
};

namespace core {

T_sp TagbodyFrame_O::_lookupTagbodyId(int depth, int index) const {
  _G();
  if (depth == 0) {
    return this->asSmartPtr();
  }
  --depth;
  return Environment_O::clasp_lookupTagbodyId(this->parentFrame(), depth, index);
}

string TagbodyFrame_O::summaryOfContents() const {
  stringstream ss;
  ss << "---" << this->_instanceClass()->classNameAsString() << "#" << this->environmentId() << std::endl;
  return (ss.str());
}

string TagbodyFrame_O::asString() const {
  return this->summaryOfContents();
}

TagbodyFrame_sp TagbodyFrame_O::create(T_sp parent) {
  _G();
  GC_ALLOCATE(TagbodyFrame_O, vf);
  vf->_ParentFrame = parent;
  return (vf);
}

EXPOSE_CLASS(core, TagbodyFrame_O);

void TagbodyFrame_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<TagbodyFrame_O>();
}

void TagbodyFrame_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, TagbodyFrame, "", "", _lisp);
#endif
}
};
