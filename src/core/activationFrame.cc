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
#include <clasp/core/array.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/wrappers.h>

namespace core {


  void error_frame_range(const char* type, int index, int capacity )
  {
    SIMPLE_ERROR(BF("Index %d out of range for %s with capacity %d") % type % index % capacity );
  }
  void error_end_of_frame_list(const char* message)
  {
    SIMPLE_ERROR(BF("Reached end of ActivationFrame list for %s") % message);
  }

void watchTriggered(T_sp *ptr) {
  printf("%s:%d Watch-pointer@%p triggered on:", __FILE__, __LINE__, ptr);
  if ((*ptr)) {
    printf("%s\n", _rep_((*ptr)).c_str());
  } else {
    printf(" UNDEFINED!!!!\n");
  }
}

T_sp ActivationFrame_O::currentVisibleEnvironment() const {
  return this->const_sharedThis<ActivationFrame_O>();
}

T_sp ActivationFrame_O::getActivationFrame() const {
  return this->const_sharedThis<ActivationFrame_O>();
}

string ActivationFrame_O::clasp_asString(T_sp af) {
  if (af.nilp()) {
    stringstream ss;
    General_sp gaf(af.unsafe_general());
    ss << "#<" << gaf->_instanceClass()->_classNameAsString() << " NIL>";
    return ((ss.str()));
  }
  return gc::As<ActivationFrame_sp>(af)->asString();
}

string ActivationFrame_O::asString() const {
  SUBIMP();
}

bool ActivationFrame_O::_findTag(Symbol_sp sym, int &depth, int &index, bool &interFunction, T_sp &tagbodyEnv) const {
  T_sp parent = clasp_currentVisibleEnvironment(this->getParentEnvironment(),false);
  ++depth;
  return clasp_findTag(parent, sym, depth, index, interFunction, tagbodyEnv);
}

bool ActivationFrame_O::_findValue(T_sp sym, int &depth, int &index, bool& crossesFunction, ValueKind &valueKind, T_sp &value, T_sp& env) const {
  T_sp parent = clasp_currentVisibleEnvironment(this->getParentEnvironment(),false);
  ++depth;
  return clasp_findValue(parent, sym, depth, index, crossesFunction, valueKind, value, env);
}

bool ActivationFrame_O::_findFunction(T_sp functionName, int &depth, int &index, Function_sp &func, T_sp& functionEnv) const {
  T_sp parent = clasp_currentVisibleEnvironment(this->getParentEnvironment(),false);
  ++depth;
  return clasp_findFunction(parent, functionName, depth, index, func, functionEnv);
}

string ActivationFrame_O::summaryOfContents() const {
  SUBCLASS_MUST_IMPLEMENT();
}

#if 0
T_sp ActivationFrame_O::_lookupTagbodyId(int depth, int index) const {
  if (depth == 0) {
    SIMPLE_ERROR(BF("Hit depth=0 and did not find value - this activation frame: %s") % this->__repr__());
  }
  --depth;
  return Environment_O::clasp_lookupTagbodyId(this->parentFrame(), depth, index);
}
#endif

#if 0
T_sp &ActivationFrame_O::lookupValueReference(int depth, int index) {
  if (depth == 0) {
    SIMPLE_ERROR(BF("Hit depth=0 and did not find value - this activation frame: %s") % this->__repr__());
  }
  --depth;
  return Environment_O::clasp_lookupValueReference(this->parentFrame(), depth, index);
}
#endif


#if 0
T_sp ActivationFrame_O::_lookupValue(int depth, int index) {
  return this->lookupValueReference(depth, index);
}
#endif

#if 0
Function_sp ActivationFrame_O::_lookupFunction(int depth, int index) const {
  if (depth == 0) {
    SIMPLE_ERROR(BF("Hit depth=0 and did not find function - this activation frame: %s") % this->__repr__());
  }
  --depth;
  return Environment_O::clasp_lookupFunction(this->parentFrame(), depth, index);
}
#endif




};

namespace core {

string ValueFrame_O::summaryOfContents() const {
  stringstream ss;
  ss << "---" << this->_instanceClass()->_classNameAsString() << " :len " << this->length() << std::endl;
  T_sp debuggingInfo = _Nil<T_O>();
  if (this->_DebuggingInfo.notnilp()) {
    debuggingInfo = gc::As<Vector_sp>(this->_DebuggingInfo);
  }
  for (int i = 0; i < this->_Objects.length(); ++i) {
    if (debuggingInfo.notnilp() && (i < cl__length(gc::As<Vector_sp>(debuggingInfo)))) {
      ss << _rep_(gc::As<Vector_sp>(debuggingInfo)->rowMajorAref(i)) << " ";
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
    ss << "Parent@" << (void*)this->_Parent.raw_();
    ss << std::endl;
  }
  return ((ss.str()));
}

string ValueFrame_O::asString() const {
  return this->summaryOfContents();
}

#if 0
T_sp &ValueFrame_O::lookupValueReference(int depth, int index) {
  if (depth == 0) {
    ASSERTF(index < this->_Objects.capacity(), BF("Out of range index %d for ValueFrame with %d entries") % index % this->_Objects.capacity());
    return ((this->_Objects[index]));
  }
  --depth;
  return Environment_O::clasp_lookupValueReference(this->parentFrame(), depth, index);
}
#endif

void ValueFrame_O::fillRestOfEntries(int istart, List_sp values) {
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

#if 0
T_sp ValueFrame_O::_lookupValue(int depth, int index) {
  return this->lookupValueReference(depth, index);
}
#endif
ValueFrame_sp ValueFrame_O::createForLambdaListHandler(LambdaListHandler_sp llh, T_sp parent) {
  ValueFrame_sp vf(ValueFrame_O::create(llh->numberOfLexicalVariables(), parent));
  return ((vf));
}

ValueFrame_sp ValueFrame_O::create(List_sp values, T_sp parent) {
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
    if (gc::As<Symbol_sp>(debuggingInfo->rowMajorAref(i)) == sym) {
      this->_Objects[i] = obj;
      return true;
    }
  }
  if (this->parentFrame().nilp()) return false;
  return clasp_updateValue(this->parentFrame(), sym, obj);
}

/*! Find the value bound to a symbol based on the symbol name.
       This is only used by the interpreter and shouldn't be expected to be fast.
    */
bool ValueFrame_O::_findValue(T_sp sym, int &depth, int &index, bool& crossesFunction, ValueKind &valueKind, T_sp &value, T_sp& env) const {
  //	printf("%s:%d ValueFrame_O::_findValue - switch to DWARF debugging to look up values\n", __FILE__, __LINE__ );
  if (this->_DebuggingInfo.nilp()) {
    return ((this->Base::_findValue(sym, depth, index, crossesFunction, valueKind, value, env)));
  }
  if (!this->_DebuggingInfo) {
    printf("%s:%d The debugging info was NULL!!!!! Why is this happening?\n",__FILE__,__LINE__);
    return ((this->Base::_findValue(sym, depth, index, crossesFunction, valueKind, value, env)));
  }
  Vector_sp debuggingInfo = gc::As<Vector_sp>(this->_DebuggingInfo);
  int i = 0;
  for (; i < this->length(); ++i) {
    if (gc::As<Symbol_sp>(debuggingInfo->rowMajorAref(i)) == sym) {
      index = i;
      value = this->_Objects[i];
      valueKind = lexicalValue;
      return true;
    }
  }
  ++depth;
  return Environment_O::clasp_findValue(this->parentFrame(), sym, depth, index, crossesFunction, valueKind, value, env);
}






string FunctionFrame_O::summaryOfContents() const {
  return (this->asString());
}

string FunctionFrame_O::asString() const {
  stringstream ss;
  ss << "#<[" << this->_instanceClass()->_classNameAsString() << " :len " << this->length() << " ";
  for (int i = 0; i < this->_Objects.length(); ++i) {
    ss << _rep_(this->_Objects[i]) << " " << std::endl;
  }
  ss << "]>";
  return ((ss.str()));
}

#if 0
Function_sp FunctionFrame_O::_lookupFunction(int depth, int index) const {
  if (depth == 0) {
    if (index >= this->_Objects.capacity()) {
      SIMPLE_ERROR(BF("Out of range index[%d] for FunctionFrame with %d entries") % index % this->_Objects.capacity());
    }
    return gc::As<Function_sp>((this->entry(index)));
  }
  --depth;
  return Environment_O::clasp_lookupFunction(this->parentFrame(), depth, index);
}
#endif

};

namespace core {

#if 0
T_sp TagbodyFrame_O::_lookupTagbodyId(int depth, int index) const {
  if (depth == 0) {
    return this->asSmartPtr();
  }
  --depth;
  return Environment_O::clasp_lookupTagbodyId(this->parentFrame(), depth, index);
}


string TagbodyFrame_O::summaryOfContents() const {
  stringstream ss;
  ss << "---" << this->_instanceClass()->_classNameAsString()
     << std::endl;
  return (ss.str());
}

string TagbodyFrame_O::asString() const {
  return this->summaryOfContents();
}

#endif

};
