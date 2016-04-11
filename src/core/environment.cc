/*
    File: environment.cc
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
//
// This is an optimization where a static_cast is used
// for the environment rather than a dynamic cast.
// beach found that a lot of time was spent in dynamic_cast if
// this is not set
#define USE_STATIC_CAST_FOR_ENVIRONMENT 1

#define DEBUG_LEVEL_FULL

#include <string.h>
#include <clasp/core/common.h>
//#i n c l u d e "stringSet.h"
#include <clasp/core/symbolTable.h>
#include <clasp/core/environment.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/standardObject.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/sequence.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/primitives.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/wrappers.h>

#define MAX_CONS_CHARS 1024

namespace core {

CL_LAMBDA(env sym);
CL_DECLARE();
CL_DOCSTRING("classifyReturnFromSymbol");
CL_DEFUN T_mv core__classify_return_from_symbol(T_sp env, Symbol_sp sym) {
  bool interFunction = false;
  return Environment_O::clasp_recognizesBlockSymbol(env, sym, interFunction);
}

CL_LAMBDA(frame);
CL_DECLARE();
CL_DOCSTRING("environmentLength - number of entries in this environment");
CL_DEFUN int core__environment_length(T_sp frame) {
  if (frame.nilp())
    return 0;
  else if (ActivationFrame_sp af = frame.asOrNull<ActivationFrame_O>()) {
    return af->length();
  }
  SIMPLE_ERROR(BF("Trying to get environment-length of something not an activation-frame"));
}

CL_LAMBDA(frame);
CL_DECLARE();
CL_DOCSTRING("environmentDebugNames - number of entries in this environment");
CL_DEFUN T_sp core__environment_debug_names(T_sp frame) {
  if (frame.nilp())
    return _Nil<T_O>();
  else if (ValueFrame_sp vf = frame.asOrNull<ValueFrame_O>()) {
    return vf->debuggingInfo();
  } else if (ActivationFrame_sp af = frame.asOrNull<ActivationFrame_O>()) {
    (void)af;
    return _Nil<T_O>();
  }
  SIMPLE_ERROR(BF("Trying to get environment-debug-names of something not an activation-frame: %s") % _rep_(frame));
}

CL_LAMBDA(frame);
CL_DECLARE();
CL_DOCSTRING("environmentDebugValues - number of entries in this environment");
CL_DEFUN T_sp core__environment_debug_values(T_sp frame) {
  if (frame.nilp())
    return _Nil<T_O>();
  else if (ValueFrame_sp vf = frame.asOrNull<ValueFrame_O>()) {
    int iEnd = vf->length();
    VectorObjects_sp vo = VectorObjects_O::create(_Nil<T_O>(), iEnd, _Nil<T_O>());
    for (int i(0); i < iEnd; ++i) {
      T_sp val = (*vf)[i];
      if (val.unboundp()) {
        val = _sym__BANG_unbound_BANG_;
      }
      vo->setf_elt(i, val);
    }
    return vo;
  } else if (ActivationFrame_sp af = frame.asOrNull<ActivationFrame_O>()) {
    (void)af;
    return _Nil<T_O>();
  }
  SIMPLE_ERROR(BF("Trying to get environment-debug-values of something not an activation-frame: %s") % _rep_(frame));
}

CL_LAMBDA(name env);
CL_DECLARE();
CL_DOCSTRING("lexicalFunction - If found return (values T fn depth index) otherwise nil");
CL_DEFUN T_mv core__lexical_function(T_sp name, T_sp env) {
  int depth = 0;
  int index = 0;
  Function_sp func;
  if (Environment_O::clasp_findFunction(env, name, depth, index, func)) {
    return Values(_lisp->_true(), func, make_fixnum(depth), make_fixnum(index));
  }
  return Values(_Nil<T_O>());
};

CL_LAMBDA(name env);
CL_DECLARE();
CL_DOCSTRING("lexicalMacroFunction - If found return (values T fn depth index) otherwise nil");
CL_DEFUN T_mv core__lexical_macro_function(T_sp name, T_sp env) {
  int depth = 0;
  int index = 0;
  Function_sp func;
  if (Environment_O::clasp_findMacro(env, name, depth, index, func)) {
    return Values(_lisp->_true(), func, make_fixnum(depth), make_fixnum(index));
  }
  return Values(_Nil<T_O>());
};

//
// What about specials??????
#define ARGS_af_updateValue "(env symbol value)"
#define DECL_af_updateValue ""
#define DOCS_af_updateValue "updateValue"
bool af_updateValue(T_sp env, Symbol_sp sym, T_sp val) {
  ASSERT(env.isA<Environment_O>());
  Environment_sp eenv = gc::reinterpret_cast_smart_ptr<Environment_O, T_O>(env);
  return eenv->_updateValue(sym, val);
};


CL_LAMBDA(env);
CL_DECLARE();
CL_DOCSTRING("environmentActivationFrame");
CL_DEFUN T_sp core__environment_activation_frame(T_sp env) {
  if (env.nilp())
    return env;
  return gc::As<Environment_sp>(env)->getActivationFrame();
};

CL_LAMBDA(env);
CL_DECLARE();
CL_DOCSTRING("Return a list of environment parents");
CL_DEFUN T_sp core__environment_list(T_sp env) {
  List_sp result = _Nil<T_O>();
  for (T_sp ecur = env; ecur.notnilp(); ecur = gc::As<Environment_sp>(ecur)->getParentEnvironment()) {
    result = Cons_O::create(ecur, result);
  }
  return (cl__nreverse(result));
};

CL_LAMBDA(env);
CL_DECLARE();
CL_DOCSTRING("Return a list of environment parents");
CL_DEFUN T_sp core__environment_type_list(T_sp env) {
  List_sp result = _Nil<T_O>();
  for (T_sp ecur = env; ecur.notnilp(); ecur = gc::As<Environment_sp>(ecur)->getParentEnvironment()) {
    result = Cons_O::create(lisp_static_class(ecur), result);
  }
  return cl__nreverse(result);
};

int Environment_O::clasp_countFunctionContainerEnvironments(T_sp env) {
  if (env.nilp())
    return 0;
  if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->countFunctionContainerEnvironments();
  }
  NOT_ENVIRONMENT_ERROR(env);
};

CL_LAMBDA(env);
CL_DECLARE();
CL_DOCSTRING("Return the RuntimeEnvironment or nil");
CL_DEFUN T_sp core__runtime_environment(T_sp tenv) {
  if (tenv.nilp())
    return _Nil<T_O>();
  if (Environment_sp env = tenv.asOrNull<Environment_O>()) {
    return env->runtimeEnvironment();
  }
  SIMPLE_ERROR(BF("No runtime environment available for %s") % _rep_(tenv));
};


CL_LISPIFY_NAME("setRuntimeEnvironment");
CL_DEFMETHOD void Environment_O::setRuntimeEnvironment(T_sp renv) {
  SIMPLE_ERROR(BF("Only RuntimeVisibleEnvironments support runtime environments"));
}

T_sp Environment_O::runtimeEnvironment() const {
  SIMPLE_ERROR(BF("Only RuntimeVisibleEnvironments support runtime environments"));
}

CL_LISPIFY_NAME("getParentEnvironment");
CL_DEFMETHOD T_sp Environment_O::getParentEnvironment() const {
  SUBIMP();
}

T_mv Environment_O::clasp_lookupMetadata(T_sp env, Symbol_sp key) {
  IMPLEMENT_MEF(BF("Checkout Environment_O::lookupMetadata - it doesn't look like we do much yet"));
}

T_sp Environment_O::clasp_getActivationFrame(T_sp tenv) {
  if (tenv.nilp())
    return (_Nil<T_O>());
  if (Environment_sp env = tenv.asOrNull<Environment_O>()) {
    return (env->getActivationFrame());
  }
  return _Nil<T_O>();
  //  NOT_ENVIRONMENT_ERROR(tenv);
};

T_sp Environment_O::getActivationFrame() const {
  SUBCLASS_MUST_IMPLEMENT();
}



  SYMBOL_SC_(CorePkg, environmentActivationFrame);
  SYMBOL_SC_(CorePkg, currentVisibleEnvironment);
  SYMBOL_SC_(CorePkg, runtimeEnvironment);
  SYMBOL_SC_(CorePkg, environmentList);
  SYMBOL_SC_(CorePkg, environmentTypeList);




//
// Constructor
//
CL_NAME(CURRENT-VISIBLE-ENVIRONMENT);
CL_DEFUN T_sp Environment_O::clasp_currentVisibleEnvironment(T_sp env) {
  if (env.nilp())
    return (_Nil<T_O>());
  if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return (eenv->currentVisibleEnvironment());
  }
  return env;
};

T_sp Environment_O::currentVisibleEnvironment() const {
  SUBIMP();
};

void Environment_O::setupParent(T_sp environ) {
}

void Environment_O::clasp_environmentStackFill(T_sp env, int level, stringstream &sout) {
  if (env.nilp()) {
    sout << "NIL";
    return;
  }
  if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    eenv->_environmentStackFill(level, sout);
    return;
  }
  NOT_ENVIRONMENT_ERROR(env);
}

void Environment_O::_environmentStackFill(int level, stringstream &sout) {
  _OF();
  sout << this->summaryOfContents();
  T_sp parent = this->getParentEnvironment();
  clasp_environmentStackFill(parent, level + 1, sout);
}

CL_LISPIFY_NAME("environmentStackAsString");
CL_DEFMETHOD string Environment_O::environmentStackAsString() {
  _OF();
  stringstream sout;
  this->_environmentStackFill(1, sout);
  return sout.str();
}

void Environment_O::dump() {
  stringstream sout;
  this->_environmentStackFill(1, sout);
  printf("%s:%d Dumping environment\n%s\n", __FILE__, __LINE__, sout.str().c_str());
}

List_sp Environment_O::clasp_gather_metadata(T_sp env, Symbol_sp key) {
  if (env.nilp())
    return _Nil<T_O>();
  else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->gather_metadata(key);
  }
  NOT_ENVIRONMENT_ERROR(env);
}

CL_LISPIFY_NAME("gather_metadata");
CL_DEFMETHOD List_sp Environment_O::gather_metadata(Symbol_sp key) const {
  if (this->getParentEnvironment().nilp())
    return _Nil<T_O>();
  return clasp_gather_metadata(this->getParentEnvironment(), key);
}

CL_LISPIFY_NAME("lookupMetadata");
CL_DEFMETHOD T_mv Environment_O::lookupMetadata(Symbol_sp key) const {
  if (this->getParentEnvironment().nilp()) {
    return (Values(_Nil<T_O>(), _Nil<T_O>(), _Nil<T_O>()));
  }
  return Environment_O::clasp_lookupMetadata(this->getParentEnvironment(), key);
}

CL_LISPIFY_NAME("localMetadata");
CL_DEFMETHOD T_mv Environment_O::localMetadata(Symbol_sp key) const {
  SUBCLASS_MUST_IMPLEMENT();
}

T_sp Environment_O::clasp_lookupValue(T_sp env, int depth, int index) {
  ASSERT(env.isA<Environment_O>());
  Environment_sp eenv = gc::reinterpret_cast_smart_ptr<Environment_O>(env);
  return eenv->_lookupValue(depth, index);
}

T_sp &Environment_O::clasp_lookupValueReference(T_sp env, int depth, int index) {
// set this to 1 to use dynamic_cast and 0 to use what is essentially a static cast
  ASSERT(env && env.isA<Environment_O>());
  Environment_sp eenv = gc::reinterpret_cast_smart_ptr<Environment_O, T_O>(env);
  return eenv->lookupValueReference(depth, index);
}

Function_sp Environment_O::clasp_lookupFunction(T_sp env, int depth, int index) {
  ASSERT(env.isA<Environment_O>());
  Environment_sp eenv = gc::reinterpret_cast_smart_ptr<Environment_O, T_O>(env);
  return eenv->_lookupFunction(depth, index);
}

T_sp Environment_O::clasp_lookupTagbodyId(T_sp env, int depth, int index) {
  ASSERT(env.isA<Environment_O>());
  Environment_sp eenv = gc::reinterpret_cast_smart_ptr<Environment_O, T_O>(env);
  return eenv->_lookupTagbodyId(depth, index);
}

T_sp Environment_O::_lookupValue(int depth, int index) {
  SUBIMP();
}

T_sp &Environment_O::lookupValueReference(int depth, int index) {
  SUBIMP();
}

Function_sp Environment_O::_lookupFunction(int depth, int index) const {
  SUBIMP();
}

string Environment_O::__repr__() const {
  stringstream ss;
  ss << "#<" << lisp_classNameAsString(cl__class_of(this->asSmartPtr())) << ">";
#if 0
	int tab = gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabSTAR->symbolValue())->get();
	{
	    ss << (BF("--------------------------- %20s -----") % this->_instanceClass()->classNameAsString() ).str() << std::endl;
	    tab += gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabIncrementSTAR->symbolValue())->get();
	    Fixnum_sp fntab = make_fixnum(tab);
	    DynamicScopeManager scope(_sym_STARenvironmentPrintingTabSTAR,fntab);
	    ss <<this->summaryOfContents();
	    if ( this->getParentEnvironment().notnilp() )
	    {
		ss << string(tab,' ') << " :parent ";
		ss << _rep_(this->getParentEnvironment());
	    }
	    ss << string(tab,' ') << " ]" << std::endl;
	}
#endif
  return ss.str();
}

bool Environment_O::_updateValue(Symbol_sp sym, T_sp obj) {
  if (this->getParentEnvironment().nilp()) {
    SIMPLE_ERROR(BF("Could not update local symbol(%s) because it was not defined") % _rep_(sym));
  }
  return af_updateValue(this->getParentEnvironment(), sym, obj);
}

bool Environment_O::findValue(T_sp sym, int &depth, int &index, ValueKind &valueKind, T_sp &value) const {
  depth = 0;
  index = -1;
  valueKind = undeterminedValue;
  return this->_findValue(sym, depth, index, valueKind, value);
}

bool Environment_O::clasp_findValue(T_sp env, T_sp sym, int &depth, int &index, ValueKind &valueKind, T_sp &value) {
  if (env.nilp()) {
    depth = -1;
    index = -1;
    valueKind = undeterminedValue;
    return false;
  }
  if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->_findValue(sym, depth, index, valueKind, value);
  }
  NOT_ENVIRONMENT_ERROR(env);
}

bool Environment_O::clasp_lexicalSpecialP(T_sp env, Symbol_sp sym) {
  if (env.nilp()) {
    return false;
  } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->lexicalSpecialP(sym);
  }
  NOT_ENVIRONMENT_ERROR(env);
}

bool Environment_O::_findValue(T_sp sym, int &depth, int &index, ValueKind &valueKind, T_sp &value) const {
  T_sp parent = clasp_currentVisibleEnvironment(this->getParentEnvironment());
  return clasp_findValue(parent, sym, depth, index, valueKind, value);
}

bool Environment_O::clasp_findFunction(T_sp env, T_sp functionName, int &depth, int &index, Function_sp &func) {
  if (env.nilp()) {
    depth = -1;
    index = -1;
    return false;
  } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->_findFunction(functionName, depth, index, func);
  }
  NOT_ENVIRONMENT_ERROR(env);
}

bool Environment_O::_findFunction(T_sp functionName, int &depth, int &index, Function_sp &func) const {
  return clasp_findFunction(this->getParentEnvironment(), functionName, depth, index, func);
}

bool Environment_O::findFunction(T_sp functionName, int &depth, int &index, Function_sp &value) const {
  depth = 0;
  index = -1;
  return this->_findFunction(functionName, depth, index, value);
}

bool Environment_O::clasp_findMacro(T_sp env, Symbol_sp sym, int &depth, int &index, Function_sp &func) {
  if (env.nilp()) {
    // Look in the global environment
    depth = -1;
    index = -1;
    //	    func = sym->symbolFunction();
    return false;
  } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->_findMacro(sym, depth, index, func);
  }
  NOT_ENVIRONMENT_ERROR(env);
}

bool Environment_O::_findMacro(Symbol_sp sym, int &depth, int &index, Function_sp &func) const {
  return clasp_findMacro(this->getParentEnvironment(), sym, depth, index, func);
}

bool Environment_O::findMacro(Symbol_sp sym, int &depth, int &index, Function_sp &value) const {
  depth = 0;
  index = -1;
  return this->_findMacro(sym, depth, index, value);
}

T_sp Environment_O::clasp_find_current_code_environment(T_sp env) {
  IMPLEMENT_ME();
}

T_mv Environment_O::clasp_recognizesBlockSymbol(T_sp env, Symbol_sp sym, bool &interFunction) {
  if (env.nilp()) {
    return Values(_Nil<T_O>(), _Nil<T_O>(), _Nil<T_O>());
  } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->recognizesBlockSymbol(sym, interFunction);
  }
  NOT_ENVIRONMENT_ERROR(env);
}

int Environment_O::clasp_getBlockSymbolFrame(T_sp env, Symbol_sp sym) {
  if (env.nilp()) {
    SIMPLE_ERROR(BF("Could not find block symbol frame for %s") % _rep_(sym));
  } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->getBlockSymbolFrame(sym);
  }
  NOT_ENVIRONMENT_ERROR(env);
}

T_sp Environment_O::clasp_find_unwindable_environment(T_sp env) {
  if (env.nilp()) {
    SIMPLE_ERROR(BF("Could not find unwindable environment"));
  } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->find_unwindable_environment();
  }
  NOT_ENVIRONMENT_ERROR(env);
}

T_sp Environment_O::clasp_find_tagbody_tag_environment(T_sp env, Symbol_sp tag) {
  if (env.nilp()) {
    SIMPLE_ERROR(BF("Could not find environment with tag[%s]") % _rep_(tag));
  } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->find_tagbody_tag_environment(tag);
  }
  NOT_ENVIRONMENT_ERROR(env);
}

T_sp Environment_O::clasp_find_block_named_environment(T_sp env, Symbol_sp blockName) {
  if (env.nilp()) {
    SIMPLE_ERROR(BF("Could not find block named environment with name[%s]") % _rep_(blockName));
  } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->find_block_named_environment(blockName);
  }
  NOT_ENVIRONMENT_ERROR(env);
}

bool Environment_O::clasp_findSymbolMacro(T_sp env, Symbol_sp sym, int &depth, int &index, bool &shadowed, Function_sp &func) {
  if (env.nilp()) {
    depth = -1;
    index = -1;
    shadowed = false;
    return false;
  } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->_findSymbolMacro(sym, depth, index, shadowed, func);
  }
  NOT_ENVIRONMENT_ERROR(env);
}

bool Environment_O::_findSymbolMacro(Symbol_sp sym, int &depth, int &index, bool &shadowed, Function_sp &func) const {
  return clasp_findSymbolMacro(this->getParentEnvironment(), sym, depth, index, shadowed, func);
}

bool Environment_O::findSymbolMacro(Symbol_sp sym, int &depth, int &index, bool &shadowed, Function_sp &value) const {
  depth = 0;
  index = -1;
  shadowed = false;
  return this->_findSymbolMacro(sym, depth, index, shadowed, value);
}

bool Environment_O::lexicalSpecialP(Symbol_sp sym) const {
  return clasp_lexicalSpecialP(this->getParentEnvironment(), sym);
}

CL_LISPIFY_NAME("classifyVariable");
CL_DEFMETHOD List_sp Environment_O::classifyVariable(T_sp sym) const {
  int depth;
  int index;
  ValueKind valueKind;
  T_sp value;
  if (this->findValue(sym, depth, index, valueKind, value)) {
    switch (valueKind) {
    case lexicalValue:
      return Cons_O::createList(ext::_sym_lexicalVar, sym, make_fixnum(depth), make_fixnum(index));
#if 0
	    case stackValue:
		return Cons_O::createList(ext::_sym_stackVar,sym,value);
#endif
    case specialValue:
      return Cons_O::create(ext::_sym_specialVar, sym);
    default:
      // Do nothing
      break;
    }
  }
  // Lexical variable was not found - return nil
  return _Nil<T_O>();
}

CL_LISPIFY_NAME("classifyTag");
CL_DEFMETHOD List_sp Environment_O::classifyTag(Symbol_sp tag) {
  int depth;
  int index;
  bool interFunction;
  T_sp tagbodyEnv;
  if (this->findTag(tag, depth, index, interFunction, tagbodyEnv)) {
    if (interFunction) {
      return Cons_O::createList(_sym_dynamicGo, make_fixnum(depth), make_fixnum(index));
    } else {
      return Cons_O::createList(_sym_localGo, make_fixnum(depth), make_fixnum(index), tagbodyEnv);
    }
  }
  SIMPLE_ERROR(BF("Could not find tag %s") % _rep_(tag));
}

CL_LISPIFY_NAME("classifyFunctionLookup");
CL_DEFMETHOD List_sp Environment_O::classifyFunctionLookup(T_sp functionName) const {
  int depth;
  int index;
  Function_sp value;
  if (this->findFunction(functionName, depth, index, value)) {
    return Cons_O::createList(_sym_lexicalFunction, functionName, make_fixnum(depth), make_fixnum(index));
  }
  // If the function was not lexical then it is automatically special
  return Cons_O::create(_sym_globalFunction, functionName);
}

T_sp Environment_O::find_current_code_environment() const {
  _OF();
  if (this->getParentEnvironment().nilp()) {
    SIMPLE_ERROR(BF("Could not find current code environment"));
  }
  return Environment_O::clasp_find_current_code_environment(this->getParentEnvironment());
}

T_mv Environment_O::recognizesBlockSymbol(Symbol_sp sym, bool &interFunction) const {
  if (this->getParentEnvironment().nilp())
    return Values(_Nil<T_O>(), _Nil<T_O>(), _Nil<T_O>());
  return Environment_O::clasp_recognizesBlockSymbol(this->getParentEnvironment(), sym, interFunction);
}

CL_LISPIFY_NAME("getBlockSymbolFrame");
CL_DEFMETHOD int Environment_O::getBlockSymbolFrame(Symbol_sp sym) const {
  return Environment_O::clasp_getBlockSymbolFrame(this->getParentEnvironment(), sym);
}

bool Environment_O::clasp_findTag(T_sp env, Symbol_sp sym, int &depth, int &index, bool &interFunction, T_sp &tagbodyEnv) {
  if (env.nilp())
    return false;
  if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->_findTag(sym, depth, index, interFunction, tagbodyEnv);
  }
  NOT_ENVIRONMENT_ERROR(env);
}

bool Environment_O::_findTag(Symbol_sp sym, int &depth, int &index, bool &interFunction, T_sp &tagbodyEnv) const {
  return clasp_findTag(this->getParentEnvironment(), sym, depth, index, interFunction, tagbodyEnv);
}

bool Environment_O::findTag(Symbol_sp sym, int &depth, int &index, bool &interFunction, T_sp &tagbodyEnv) const {
  depth = 0;
  index = 0;
  interFunction = false;
  tagbodyEnv = _Nil<T_O>();
  return this->_findTag(sym, depth, index, interFunction, tagbodyEnv);
}

CL_LISPIFY_NAME("countFunctionContainerEnvironments");
CL_DEFMETHOD int Environment_O::countFunctionContainerEnvironments() const {
  return clasp_countFunctionContainerEnvironments(this->getParentEnvironment());
}

CL_LISPIFY_NAME("find_block_named_environment");
CL_DEFMETHOD T_sp Environment_O::find_block_named_environment(Symbol_sp blockName) const {
  _OF();
  T_sp parent = this->getParentEnvironment();
  if (parent.nilp()) {
    SIMPLE_ERROR(BF("Could not find block with name[%s]") % _rep_(blockName));
  }
  return gc::As<Environment_sp>(parent)->find_block_named_environment(blockName);
}

CL_LISPIFY_NAME("find_unwindable_environment");
CL_DEFMETHOD T_sp Environment_O::find_unwindable_environment() const {
  _OF();
  return Environment_O::clasp_find_unwindable_environment(this->getParentEnvironment());
}

CL_LISPIFY_NAME("find_tagbody_tag_environment");
CL_DEFMETHOD T_sp Environment_O::find_tagbody_tag_environment(Symbol_sp tag) const {
  _OF();
  return Environment_O::clasp_find_tagbody_tag_environment(this->getParentEnvironment(), tag);
}

string Environment_O::clasp_summaryOfContents(T_sp env) {
  int tab = unbox_fixnum(gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabSTAR->symbolValue()));
  stringstream ss;
  if (env.nilp()) {
    ss << string(tab, ' ') << "#<Environment nil>" << std::endl;
    return ss.str();
  }
  if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->summaryOfContents();
  }
  NOT_ENVIRONMENT_ERROR(env);
}

string Environment_O::summaryOfContents() const {
  int tab = unbox_fixnum(gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabSTAR->symbolValue()));
  stringstream ss;
  ss << string(tab, ' ') << "#<Environment_O::-no-contents->" << std::endl;
  return ss.str();
}

#if 0
    /*! If the value is lexical return (values val T).
      If the value is locally special return (values T nil).
      If the variable is not found return (values nil nil) */
    T_mv Environment_O::variable_lookup(Symbol_sp sym) const
    {
	int depth, index;
	bool special;
	T_sp value;
	bool found = this->findValue(sym,depth,index,special,value);
	if ( !found )
	{
	    return(Values(_lisp->_boolean(special),_Nil<T_O>()));
	}
	return(Values(value,_lisp->_true()));
    }
#endif

#if 0
    T_mv Environment_O::variable_lookup(const string& package, const string& symStr) const
    {
	// TODO: Ditch this function - we shouldn't lookup symbols like this
	Symbol_sp sym = _lisp->internWithPackageName(package,symStr);
	return this->variable_lookup(sym);
    }
#endif

#if 0
    Function_sp Environment_O::function_lookup(T_sp functionName)
    {
	int depth, index;
	Function_sp func;
	if (this->findFunction(functionName,depth,index,func) )
	{
	    return func;
	}
	return _Nil<Function_O>();
    }
#endif

#if 0

    Function_sp Environment_O::lookupSymbolMacro(Symbol_sp sym, bool& foundIt) const
    {
	LOG(BF("Looking to see if there is a symbol-macro with name(%s)") % _rep_(sym) );
	ASSERTNOTNULL(this->getParentEnvironment());
	if ( this->getParentEnvironment().nilp() )
	{
	    // There is no symbol-macro with this name, return nil/false
	    foundIt = false;
	    return lisp()->nil<Function_O>();
	}
	return this->getParentEnvironment()->lookupSymbolMacro(sym,foundIt);
    }

#endif

LexicalEnvironment_O::LexicalEnvironment_O() : Base(){};



void LexicalEnvironment_O::initialize() {
  this->Base::initialize();
  this->_Metadata = HashTableEq_O::create_default();
}




T_sp LexicalEnvironment_O::setf_metadata(Symbol_sp key, T_sp val) {
  this->_Metadata->hash_table_setf_gethash(key, val);
  return val;
};

void LexicalEnvironment_O::setupParent(T_sp environ) {
  this->_ParentEnvironment = environ;
  this->Base::setupParent(environ);
}

T_sp LexicalEnvironment_O::getParentEnvironment() const {
  _OF();
  ASSERTNOTNULL(this->_ParentEnvironment);
  return this->_ParentEnvironment;
}

string LexicalEnvironment_O::summaryOfContents() const {
  int tab = unbox_fixnum(gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabSTAR->symbolValue()));
  stringstream ss;
  if (this->_Metadata->hashTableSize() > 0) {
    ss << string(tab, ' ') << "----Metadata follows ---" << std::endl;
    this->_Metadata->mapHash([tab, &ss](T_sp key, T_sp val) {
                    ss << string(tab,' ')<< _rep_(key) << " --> " << _rep_(val) << std::endl;
    });
    ss << string(tab, ' ') << "-----Metadata done ----" << std::endl;
  } else {
    ss << string(tab, ' ') << "----NO METADATA----" << std::endl;
  }
  return ss.str();
}

List_sp LexicalEnvironment_O::gather_metadata(Symbol_sp key) const {
  List_sp parentGathered = _Nil<List_V>();
  if (this->getParentEnvironment().notnilp()) {
    parentGathered = clasp_gather_metadata(this->getParentEnvironment(), key);
  }
  List_sp keyValue = this->_Metadata->find(key);
  if (keyValue.notnilp()) {
    return Cons_O::create(oCdr(keyValue), parentGathered);
  }
  return parentGathered;
}

List_sp LexicalEnvironment_O::push_metadata(Symbol_sp key, T_sp val) {
  Cons_sp one = Cons_O::create(val, this->localMetadata(key));
  this->_Metadata->hash_table_setf_gethash(key, one);
  return one;
}

T_mv LexicalEnvironment_O::localMetadata(Symbol_sp key) const {
  List_sp it = this->_Metadata->find(key);
  if (it.nilp()) {
    return (Values(_Nil<T_O>(), _Nil<T_O>()));
  }
  return (Values(oCdr(it), _lisp->_true()));
}

T_mv LexicalEnvironment_O::lookupMetadata(Symbol_sp key) const {
  List_sp it = this->_Metadata->find(key);
  if (it.nilp()) {
    if (this->_ParentEnvironment.nilp()) {
      return (Values(_Nil<T_O>(), _Nil<T_O>(), _Nil<T_O>()));
    }
    return gc::As<Environment_sp>(this->_ParentEnvironment)->lookupMetadata(key);
  }
  return (Values(oCdr(it), _lisp->_true(), this->const_sharedThis<Environment_O>()));
}






RuntimeVisibleEnvironment_O::RuntimeVisibleEnvironment_O() : Base(){};

T_sp RuntimeVisibleEnvironment_O::currentVisibleEnvironment() const {
  //	if ( this -> isNil() ) return _Nil<T_O>();
  return this->const_sharedThis<Environment_O>();
}

bool RuntimeVisibleEnvironment_O::_findTag(Symbol_sp sym, int &depth, int &index, bool &interFunction, T_sp &tagbodyEnv) const {
  T_sp parent = this->getParentEnvironment(); // clasp_currentVisibleEnvironment(this->getParentEnvironment());
  ++depth;
  return clasp_findTag(parent, sym, depth, index, interFunction, tagbodyEnv);
}

bool RuntimeVisibleEnvironment_O::_findValue(T_sp sym, int &depth, int &index, ValueKind &valueKind, T_sp &value) const {
  T_sp parent = clasp_currentVisibleEnvironment(this->getParentEnvironment());
  ++depth;
  return clasp_findValue(parent, sym, depth, index, valueKind, value);
}

bool RuntimeVisibleEnvironment_O::_findFunction(T_sp functionName, int &depth, int &index, Function_sp &func) const {
  //	if (this -> isNil()) return false;
  T_sp parent = clasp_currentVisibleEnvironment(this->getParentEnvironment());
  LOG(BF("Moving down a level"));
  ++depth;
  return clasp_findFunction(parent, functionName, depth, index, func);
}

void ValueEnvironment_O::initialize() {
  this->Base::initialize();
  this->_SymbolIndex = HashTableEq_O::create_default();
}

bool ValueEnvironment_O::lexicalSpecialP(Symbol_sp sym) const {
  // Lookup the symbol in our list Symbol map
  List_sp fi = this->_SymbolIndex->find(sym);
  if (fi.nilp()) {
    // if we don't find it then invoke Environment_O::lexicalSpecialP
    return this->Base::lexicalSpecialP(sym);
  }
  // If the target index is a SPECIAL_TARGET then return true otherwise false
  return (unbox_fixnum(gc::As<Fixnum_sp>(oCdr(fi))) == SPECIAL_TARGET);
}

T_sp ValueEnvironment_O::getActivationFrame() const {
  //	if ( this -> isNil()) return _Nil<ActivationFrame_O>();
  return this->_ActivationFrame;
}

T_sp ValueEnvironment_O::_lookupValue(int depth, int index) {
  ValueFrame_sp vframe = gctools::As_unsafe<ValueFrame_sp>(this->_ActivationFrame);
  if (depth == 0) {
    return vframe->entry(index);
  }
  T_sp parent = clasp_currentVisibleEnvironment(this->getParentEnvironment());
  if (parent.nilp()) {
    SIMPLE_ERROR(BF("Ran out of parent environments - could not find value"));
  }
  return Environment_O::clasp_lookupValue(parent, depth - 1, index);
}

CL_LISPIFY_NAME("valueEnvironment_defineLexicalBinding");
CL_DEFMETHOD void ValueEnvironment_O::defineLexicalBinding(Symbol_sp sym, int idx) {
  List_sp it = this->_SymbolIndex->find(sym);
  if (it.notnilp()) {
#if 0
    if ( idx != gc::As<Fixnum_sp>(oCdr(it))->get() ) {
      SIMPLE_ERROR(BF("The lexical variable[%s] is already defined with index[%d] - we tried to set it to[%d]") % _rep_(sym) % _rep_(oCdr(it)) % idx );
    }
    return;
#endif
  }
  this->_SymbolIndex->hash_table_setf_gethash(sym, make_fixnum(idx));
}

CL_LISPIFY_NAME("valueEnvironment_defineSpecialBinding");
CL_DEFMETHOD void ValueEnvironment_O::defineSpecialBinding(Symbol_sp sym) {
  List_sp it = this->_SymbolIndex->find(sym);
  if (it.notnilp()) {
    if (SPECIAL_TARGET != unbox_fixnum(gc::As<Fixnum_sp>(oCdr(it)))) {
      SIMPLE_ERROR(BF("The lexical variable[%s] is already defined idx[%s]  - we tried to set it to special") % _rep_(sym) % _rep_(oCdr(it)));
    }
    return;
  }
  this->_SymbolIndex->hash_table_setf_gethash(sym, make_fixnum(SPECIAL_TARGET));
}

bool ValueEnvironment_O::_findValue(T_sp sym, int &depth, int &index, ValueKind &valueKind, T_sp &value) const {
  LOG(BF("Looking for binding for symbol(%s)") % _rep_(sym));
  //    LOG(BF("The frame stack is %d deep") % this->depth() );
  List_sp fi = this->_SymbolIndex->find(sym);
  if (fi.nilp()) {
    return this->Base::_findValue(sym, depth, index, valueKind, value);
  }
  index = unbox_fixnum(gc::As<Fixnum_sp>(oCdr(fi)));
  if (index < 0) {
    valueKind = specialValue;
    return true; // This was returning false for special values
  }
  valueKind = lexicalValue;
  LOG(BF(" Found binding %s") % fi->second);
  ValueFrame_sp vframe = gctools::As_unsafe<ValueFrame_sp>(this->_ActivationFrame);
  value = vframe->entry(index);
  return true;
}

bool ValueEnvironment_O::_findSymbolMacro(Symbol_sp sym, int &depth, int &index, bool &shadowed, Function_sp &fn) const {
  LOG(BF("Looking for binding for symbol(%s)") % _rep_(sym));
  //    LOG(BF("The frame stack is %d deep") % this->depth() );
  List_sp fi = this->_SymbolIndex->find(sym);
  if (fi.nilp()) {
    return this->Base::_findSymbolMacro(sym, depth, index, shadowed, fn);
  }
  index = unbox_fixnum(gc::As<Fixnum_sp>(oCdr(fi)));
  shadowed = true;
  return false;
}

bool ValueEnvironment_O::activationFrameElementBoundP(int idx) const {
  ValueFrame_sp vframe = gctools::As_unsafe<ValueFrame_sp>(this->_ActivationFrame);
  return vframe->boundp_entry(idx);
}

CL_LISPIFY_NAME(makeValueEnvironment);
CL_DEFUN ValueEnvironment_sp ValueEnvironment_O::createForLambdaListHandler(LambdaListHandler_sp llh, T_sp parent) {
  ValueEnvironment_sp env(ValueEnvironment_O::create());
  env->setupForLambdaListHandler(llh, parent);
  return env;
}

CL_LISPIFY_NAME(makeValueEnvironmentForNumberOfEntries);
CL_DEFUN ValueEnvironment_sp ValueEnvironment_O::createForNumberOfEntries(int numberOfArguments, T_sp parent) {
  ValueEnvironment_sp env(ValueEnvironment_O::create());
  env->setupParent(parent);
  env->_ActivationFrame = ValueFrame_O::create(numberOfArguments, clasp_getActivationFrame(clasp_currentVisibleEnvironment(parent)));
  return env;
}

CL_LISPIFY_NAME(makeValueEnvironmentForLocallySpecialEntries);
CL_DEFUN ValueEnvironment_sp ValueEnvironment_O::createForLocallySpecialEntries(List_sp specials, T_sp parent) {
  ValueEnvironment_sp env(ValueEnvironment_O::create());
  env->setupParent(parent);
  env->_ActivationFrame = ValueFrame_O::create(0, clasp_getActivationFrame(clasp_currentVisibleEnvironment(parent)));
  for (auto cur : specials) {
    env->defineSpecialBinding(gc::As<Symbol_sp>(oCar(cur)));
  }
  return env;
}

void ValueEnvironment_O::setupForLambdaListHandler(LambdaListHandler_sp llh, T_sp parent) {
  List_sp classifiedSymbols = llh->classifiedSymbols();
  this->setupParent(parent);
  int numberOfLexicals = 0;
  for (auto cur : classifiedSymbols) {
    List_sp classifiedSymbol = coerce_to_list(oCar(cur));
    Symbol_sp classification = gc::As<Symbol_sp>(oCar(classifiedSymbol));
    if (classification == ext::_sym_lexicalVar) {
      ++numberOfLexicals;
    } else if (classification == ext::_sym_specialVar) {
      // handle special declarations
      Symbol_sp sym = gc::As<Symbol_sp>(oCdr(classifiedSymbol));
      this->defineSpecialBinding(sym);
    }
  }
#if 0
  if (numberOfLexicals>0 ) {
    printf("%s:%d  Creating ValueFrame_O with numberOfLexicals: %d\n", __FILE__, __LINE__, numberOfLexicals );
  }
#endif
  this->_ActivationFrame = ValueFrame_O::create(numberOfLexicals, clasp_getActivationFrame(clasp_currentVisibleEnvironment(parent)));
}

string ValueEnvironment_O::summaryOfContents() const {
  int tab = unbox_fixnum(gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabSTAR->symbolValue()));
  stringstream ss;
  ss << "ValueEnvironment_O" << std::endl;
  ValueFrame_sp vframe = gctools::As_unsafe<ValueFrame_sp>(this->_ActivationFrame);
  this->_SymbolIndex->mapHash([this, tab, &vframe, &ss](T_sp key, T_sp value) {
                int ivalue = unbox_fixnum(gc::As<Fixnum_sp>(value));
                ss << string(tab,' ') << _rep_(key) << "#" << ivalue << " -> ";
                if ( ivalue == SPECIAL_TARGET ) {
                    ss << "SPECIAL-VAR";
                } else if ( ivalue >= cl__length(vframe) ) {
                    ss << "ActivationFrame->index["<<ivalue<<"]->OUT-OF-RANGE";
                } else if ( vframe->boundp_entry(ivalue) ) {
                    ss << _rep_(vframe->entry(ivalue));
                } else {
                    ss << "UNBOUND ";
                }
                ss << std::endl;
  });
  ss << this->Base::summaryOfContents();
  return ss.str();
}

/*! If the symbol is not in the lexical environment then throw an exception.
      If the symbol is lexical and was updated return true.
      If the symbol is locally special then don't update it (caller is responsible for doing that) and return false.
    */
bool ValueEnvironment_O::_updateValue(Symbol_sp sym, T_sp obj) {
  List_sp it = this->_SymbolIndex->find(sym);
  if (it.nilp()) {
    T_sp parent = this->getParentEnvironment();
    if (parent.nilp()) {
      SIMPLE_ERROR(BF("Could not update local symbol(%s) because it was not defined") % _rep_(sym));
    }
    return af_updateValue(clasp_currentVisibleEnvironment(parent), sym, obj);
  }
  int ivalue = unbox_fixnum(gc::As<Fixnum_sp>(oCdr(it)));
  if (ivalue < 0) {
    //	    sym->setf_symbolValue(obj);
    return false;
  }
  ValueFrame_sp vframe = gctools::As_unsafe<ValueFrame_sp>(this->_ActivationFrame);
  vframe->set_entry(ivalue, obj);
  return true;
}

T_sp ValueEnvironment_O::new_binding(Symbol_sp sym, int idx, T_sp obj) {
  if (idx < 0) {
    IMPLEMENT_MEF(BF("new_binding for special symbol[%s]") % _rep_(sym));
  }
#if 0
  if (this->_SymbolIndex->find(sym).notnilp()) {
    SIMPLE_ERROR(BF("The symbol[%s] is already in the environment") % _rep_(sym));
  }
#endif
  this->_SymbolIndex->hash_table_setf_gethash(sym, make_fixnum(idx));
  ValueFrame_sp vframe = gctools::As_unsafe<ValueFrame_sp>(this->_ActivationFrame);
  vframe->set_entry(idx, obj);
  return obj;
}











T_sp FunctionValueEnvironment_O::getActivationFrame() const {
  //	if (this -> isNil() ) return _Nil<ActivationFrame_O>();
  return this->_FunctionFrame;
};

bool FunctionValueEnvironment_O::_findFunction(T_sp functionName, int &depth, int &index, Function_sp &value) const {
  LOG(BF("Looking for binding for function name[%s]") % _rep_(functionName));
  //    LOG(BF("The frame stack is %d deep") % this->depth() );
  T_mv mv = this->_FunctionIndices->gethash(functionName, _Nil<T_O>());
  T_sp val = mv;
  bool foundp = mv.valueGet_(1).isTrue();
  if (!foundp)
    return this->Base::_findFunction(functionName, depth, index, value);
  index = unbox_fixnum(gc::As<Fixnum_sp>(val));
  LOG(BF(" Found binding %d") % index);
  T_sp tvalue = this->_FunctionFrame->entry(index);
  ASSERT(tvalue.notnilp());
  value = gc::As<Function_sp>(tvalue);
  return true;
}

//
// Constructor
//

FunctionValueEnvironment_sp FunctionValueEnvironment_O::createEmpty(T_sp parent) {
  GC_ALLOCATE(FunctionValueEnvironment_O, environ);
  environ->setupParent(parent);
  return environ;
}

CL_LISPIFY_NAME(makeFunctionValueEnvironment);
CL_DEFUN FunctionValueEnvironment_sp FunctionValueEnvironment_O::createForEntries(int numEntries, T_sp parent) {
  FunctionValueEnvironment_sp environ(FunctionValueEnvironment_O::createEmpty(parent));
  environ->_FunctionFrame = FunctionFrame_O::create(numEntries, clasp_getActivationFrame(clasp_currentVisibleEnvironment(parent)));
  return environ;
}

void FunctionValueEnvironment_O::initialize() {
  this->Base::initialize();
  this->_FunctionIndices = HashTableEqual_O::create_default();
}

#if defined(XML_ARCHIVE)
void FunctionValueEnvironment_O::archiveBase(ArchiveP node) {
  IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)

class FunctionValueMapper : public KeyValueMapper {
public:
  stringstream ss;
  int tab;
  FunctionValueEnvironment_O const &_env;

  FunctionValueMapper(int t, FunctionValueEnvironment_O const &env) : tab(t), _env(env){};
  virtual bool mapKeyValue(T_sp key, T_sp value) {
    this->ss << string(this->tab, ' ') << _rep_(key) << "#" << _rep_(value);
    int idx = unbox_fixnum(gc::As<Fixnum_sp>(value));
    ss << " -> ";
    FunctionFrame_sp fframe = gc::As<FunctionFrame_sp>(this->_env.getActivationFrame());
    T_sp entry = fframe->entry(idx);
    if (entry.nilp()) {
      ss << "NIL";
    } else if (entry.unboundp()) {
      ss << "UNBOUND";
    } else {
      Function_sp func = gc::As<Function_sp>(entry);
      ss << "function " << _rep_(func->name());
    }
    ss << std::endl;
    return true;
  }
};

string FunctionValueEnvironment_O::summaryOfContents() const {
  int tab = unbox_fixnum(gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabSTAR->symbolValue()));
  FunctionValueMapper mapper(tab, *this);
  this->_FunctionIndices->lowLevelMapHash(&mapper);
  stringstream ss;
  ss << mapper.ss.str();
  ss << this->Base::summaryOfContents();
  return ss.str();
}

CL_LISPIFY_NAME("bindFunction");
CL_DEFMETHOD int FunctionValueEnvironment_O::bind_function(T_sp functionName, Function_sp form) {
  ASSERT(form.notnilp());
  int nextIdx = this->_FunctionIndices->hashTableCount();
  this->_FunctionIndices->hash_table_setf_gethash(functionName, make_fixnum(nextIdx));
  this->_FunctionFrame->set_entry(nextIdx, form);
  return nextIdx;
}






CompileTimeEnvironment_O::CompileTimeEnvironment_O() : Base(){};

T_sp CompileTimeEnvironment_O::getActivationFrame() const {
  return clasp_getActivationFrame(this->currentVisibleEnvironment());
};

T_sp CompileTimeEnvironment_O::currentVisibleEnvironment() const {
  T_sp parent = this->getParentEnvironment();
  if (parent.nilp())
    return _Nil<T_O>();
  return clasp_currentVisibleEnvironment(parent);
}

bool CompileTimeEnvironment_O::_findValue(T_sp sym, int &depth, int &index, ValueKind &valueKind, T_sp &value) const {
  T_sp parent = clasp_currentVisibleEnvironment(this->getParentEnvironment());
  return clasp_findValue(parent, sym, depth, index, valueKind, value);
}

CL_LISPIFY_NAME(makeUnwindProtectEnvironment);
CL_DEFUN UnwindProtectEnvironment_sp UnwindProtectEnvironment_O::make(List_sp cleanupForm, T_sp parent) {
  UnwindProtectEnvironment_sp environ = UnwindProtectEnvironment_O::create();
  environ->_CleanupForm = cleanupForm;
  environ->setupParent(parent);
  return environ;
}






T_sp UnwindProtectEnvironment_O::find_unwindable_environment() const {
  _OF();
  return this->const_sharedThis<Environment_O>();
}

string UnwindProtectEnvironment_O::summaryOfContents() const {
  //	int tab = _sym_STARenvironmentPrintingTabSTAR->symbolValue().as<Fixnum_O>()->get();
  stringstream ss;
  ss << "CleanupForm: " << _rep_(this->_CleanupForm) << std::endl;
  ss << this->Base::summaryOfContents();
  return ss.str();
}

void UnwindProtectEnvironment_O::initialize() {
  this->Base::initialize();
}

#if defined(XML_ARCHIVE)
void UnwindProtectEnvironment_O::archiveBase(ArchiveP node) {
  IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)

BlockEnvironment_sp BlockEnvironment_O::create(T_sp parent) {
  BlockEnvironment_sp environ = BlockEnvironment_O::create();
  environ->setupParent(parent);
  return environ;
}

CL_LISPIFY_NAME(makeBlockEnvironment);
CL_DEFUN BlockEnvironment_sp BlockEnvironment_O::make(Symbol_sp blockSymbol, T_sp parent) {
  BlockEnvironment_sp environ = BlockEnvironment_O::create(parent);
  environ->setBlockSymbol(blockSymbol);
  return environ;
}






string BlockEnvironment_O::summaryOfContents() const {
  int tab = unbox_fixnum(gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabSTAR->symbolValue()));
  stringstream ss;
  ss << string(tab, ' ') << (BF("    :block-name %s\n") % _rep_(this->getBlockSymbol())).str();
  ss << this->Base::summaryOfContents();
  return ss.str();
}

void BlockEnvironment_O::initialize() {
  this->Base::initialize();
}

#if defined(XML_ARCHIVE)
void BlockEnvironment_O::archiveBase(ArchiveP node) {
  IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)

T_sp BlockEnvironment_O::find_block_named_environment(Symbol_sp blockName) const {
  _OF();
  if (this->getBlockSymbol() == blockName)
    return this->const_sharedThis<BlockEnvironment_O>();
  return clasp_find_block_named_environment(this->getParentEnvironment(), blockName);
}

T_mv BlockEnvironment_O::recognizesBlockSymbol(Symbol_sp sym, bool &interFunction) const {
  if (this->_BlockSymbol == sym)
    return Values(_lisp->_true(), _lisp->_boolean(interFunction), this->asSmartPtr());
  return clasp_recognizesBlockSymbol(this->getParentEnvironment(), sym, interFunction);
}
#if 0
    int BlockEnvironment_O::getBlockSymbol(Symbol_sp sym) const
    {
	if ( this->_BlockSymbol == sym ) return this->_Frame;
	if ( this->getParentEnvironment().nilp() )
	{
	    SIMPLE_ERROR(BF("Could not find block symbol %s") % _rep_(sym) );
	}
	return this->getParentEnvironment()->getBlockSymbolFrame(sym);
    }

#endif

CL_LISPIFY_NAME(makeCatchEnvironment);
CL_DEFUN CatchEnvironment_sp CatchEnvironment_O::make(T_sp parent) {
  CatchEnvironment_sp environ = CatchEnvironment_O::create();
  environ->setupParent(parent);
  return environ;
}






string CatchEnvironment_O::summaryOfContents() const {
  //	int tab = gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabSTAR->symbolValue())->get();
  stringstream ss;
  ss << this->Base::summaryOfContents();
  return ss.str();
}

void CatchEnvironment_O::initialize() {
  this->Base::initialize();
}

#if defined(XML_ARCHIVE)
void CatchEnvironment_O::archiveBase(ArchiveP node) {
  IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)

FunctionContainerEnvironment_sp FunctionContainerEnvironment_O::create(T_sp parent) {
  FunctionContainerEnvironment_sp environ = FunctionContainerEnvironment_O::create();
  environ->setupParent(parent);
  return environ;
}

CL_LISPIFY_NAME(makeFunctionContainerEnvironment);
CL_DEFUN FunctionContainerEnvironment_sp FunctionContainerEnvironment_O::make(T_sp parent) {
  FunctionContainerEnvironment_sp environ = FunctionContainerEnvironment_O::create(parent);
  return environ;
}






string FunctionContainerEnvironment_O::summaryOfContents() const {
  stringstream ss;
  //	int tab = _sym_STARenvironmentPrintingTabSTAR->symbolValue().as<Fixnum_O>()->get();
  ss << this->Base::summaryOfContents();
  return ss.str();
}

void FunctionContainerEnvironment_O::initialize() {
  this->Base::initialize();
}

T_sp FunctionContainerEnvironment_O::find_current_code_environment() const {
  _OF();
  return this->const_sharedThis<FunctionContainerEnvironment_O>();
}

int FunctionContainerEnvironment_O::countFunctionContainerEnvironments() const {
  return clasp_countFunctionContainerEnvironments(this->getParentEnvironment()) + 1;
}

bool FunctionContainerEnvironment_O::_findTag(Symbol_sp sym, int &depth, int &index, bool &interFunction, T_sp &tagbodyEnv) const {
  // We are crossing a function boundary - set interFunction to true
  //	printf("%s:%d searched through FunctionContainerEnvironment_O\n", __FILE__, __LINE__ );
  interFunction = true;
  if (this->getParentEnvironment().nilp()) {
    return false;
  }
  return clasp_findTag(this->getParentEnvironment(), sym, depth, index, interFunction, tagbodyEnv);
}

T_mv FunctionContainerEnvironment_O::recognizesBlockSymbol(Symbol_sp sym, bool &interFunction) const {
  interFunction = true;
  return clasp_recognizesBlockSymbol(this->getParentEnvironment(), sym, interFunction);
}

//
// Constructor
//

//
// Destructor
//

CL_LISPIFY_NAME(makeTagbodyEnvironment);
CL_DEFUN TagbodyEnvironment_sp TagbodyEnvironment_O::make(T_sp parent) {
  TagbodyEnvironment_sp environ = TagbodyEnvironment_O::create();
  environ->setupParent(parent);
  environ->_ActivationFrame = TagbodyFrame_O::create(clasp_getActivationFrame(clasp_currentVisibleEnvironment(parent)));
  return environ;
}






string TagbodyEnvironment_O::summaryOfContents() const {
  int tab = unbox_fixnum(gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabSTAR->symbolValue()));
  stringstream ss;
  ss << ":tagbody-id " << (void *)(gc::As<TagbodyFrame_sp>(this->getActivationFrame()).get()) << std::endl;
  this->_Tags->mapHash([tab, &ss](T_sp key, T_sp value) {
                ss << string(tab,' ') << " :tag " << _rep_(key) << std::endl;
  });
  ss << this->Base::summaryOfContents();
  return ss.str();
}

void TagbodyEnvironment_O::initialize() {
  this->Base::initialize();
  this->_Tags = HashTableEq_O::create_default();
}

CL_LISPIFY_NAME("addTag");
CL_DEFMETHOD int TagbodyEnvironment_O::addTag(Symbol_sp tag, List_sp ip) {
  _OF();
  ASSERTF(this->_Tags->find(tag).nilp(), BF("The tag[%s] has already been defined in this tagbody"));
  int index = this->_TagCode.size();
  this->_Tags->hash_table_setf_gethash(tag, make_fixnum(index));
  this->_TagCode.push_back(ip);
  return index;
};

List_sp TagbodyEnvironment_O::find(Symbol_sp tag) const {
  _OF();
  DEPRECIATED();
  return this->_Tags->find(tag);
}

string TagbodyEnvironment_O::tagsAsString() const {
  _OF();
  stringstream ss;
  this->_Tags->mapHash([&ss](T_sp key, T_sp value) {
                ss << _rep_(key) << " ";
  });
  return ss.str();
}

T_sp TagbodyEnvironment_O::getActivationFrame() const {
  return this->_ActivationFrame;
}

bool TagbodyEnvironment_O::_findTag(Symbol_sp sym, int &depth, int &index, bool &interFunction, T_sp &tagbodyEnv) const {
  //	printf("%s:%d searched through TagbodyEnvironment_O\n", __FILE__, __LINE__ );
  List_sp it = this->_Tags->find(sym);
  if (it.notnilp()) {
    index = unbox_fixnum(gc::As<Fixnum_sp>(oCdr(it)));
    tagbodyEnv = this->asSmartPtr();
    return true;
  }
  if (this->getParentEnvironment().nilp()) {
    return false;
  }
  ++depth;
  return clasp_findTag(this->getParentEnvironment(), sym, depth, index, interFunction, tagbodyEnv);
}

List_sp TagbodyEnvironment_O::codePos(int index) const {
  ASSERT(index >= 0 && index < this->_TagCode.size());
  return this->_TagCode[index];
}

T_sp TagbodyEnvironment_O::find_tagbody_tag_environment(Symbol_sp tag) const {
  _OF();
  List_sp it = this->_Tags->find(tag);
  if (it.notnilp()) {
    return this->const_sharedThis<TagbodyEnvironment_O>();
  }
  return clasp_find_tagbody_tag_environment(this->getParentEnvironment(), tag);
}

GlueEnvironment_sp GlueEnvironment_O::create(List_sp parts) {
  GlueEnvironment_sp env(GlueEnvironment_O::create());
  ql::list args(_lisp);
  for (List_sp cur = parts; cur.notnilp(); cur = oCdr(oCdr(cur))) {
    Symbol_sp sym = gc::As<Symbol_sp>(oCar(cur));
    T_sp val = oCadr(cur);
    env->_Map->hash_table_setf_gethash(sym, val);
    args << val;
  }
  env->_Args = args.cons();
  return env;
}

//
// Constructor
//

//
// Destructor
//

CL_LISPIFY_NAME(makeMacroletEnvironment);
CL_DEFUN MacroletEnvironment_sp MacroletEnvironment_O::make(T_sp parent) {
  MacroletEnvironment_sp environ = MacroletEnvironment_O::create();
  environ->setupParent(parent);
  return environ;
}






void MacroletEnvironment_O::initialize() {
  this->Base::initialize();
  this->_Macros = HashTableEq_O::create_default();
}

string MacroletEnvironment_O::summaryOfContents() const {
  int tab = unbox_fixnum(gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabSTAR->symbolValue()));
  stringstream ss;
  this->_Macros->mapHash([tab, &ss](T_sp key, T_sp value) {
                ss << string(tab,' ') << _rep_(key) << std::endl;
  });
  ss << this->Base::summaryOfContents();
  return ss.str();
}

bool MacroletEnvironment_O::_findMacro(Symbol_sp sym, int &depth, int &index, Function_sp &value) const {
  LOG(BF("Looking for binding for symbol(%s)") % _rep_(sym));
  //    LOG(BF("The frame stack is %d deep") % this->depth() );
  List_sp fi = this->_Macros->find(sym);
  if (fi.nilp()) {
    return this->Base::_findMacro(sym, depth, index, value);
  }
  LOG(BF(" Found binding %s") % fi->second);
  value = gc::As<Function_sp>(oCdr(fi));
  return true;
}

CL_LISPIFY_NAME("addMacro");
CL_DEFMETHOD void MacroletEnvironment_O::addMacro(Symbol_sp sym, Function_sp macro) {
  this->_Macros->hash_table_setf_gethash(sym, macro);
}

CL_LISPIFY_NAME(makeSymbolMacroletEnvironment);
CL_DEFUN SymbolMacroletEnvironment_sp SymbolMacroletEnvironment_O::make(T_sp parent) {
  SymbolMacroletEnvironment_sp environ = SymbolMacroletEnvironment_O::create();
  environ->setupParent(parent);
  return environ;
}

bool SymbolMacroletEnvironment_O::_findSymbolMacro(Symbol_sp sym, int &depth, int &index, bool &shadowed, Function_sp &value) const {
  LOG(BF("Looking for binding for symbol(%s)") % _rep_(sym));
  //    LOG(BF("The frame stack is %d deep") % this->depth() );
  List_sp fi = this->_Macros->find(sym);
  if (fi.nilp()) {
    return this->Base::_findSymbolMacro(sym, depth, index, shadowed, value);
  }
  LOG(BF(" Found binding %s") % fi->second);
  value = gc::As<Function_sp>(oCdr(fi));
  shadowed = false;
  return true;
}

CL_LISPIFY_NAME("addSymbolMacro");
CL_DEFMETHOD void SymbolMacroletEnvironment_O::addSymbolMacro(Symbol_sp sym, Function_sp expansion) {
  this->_Macros->hash_table_setf_gethash(sym, expansion);
}






void SymbolMacroletEnvironment_O::initialize() {
  this->Base::initialize();
  this->_Macros = HashTableEq_O::create_default();
}

string SymbolMacroletEnvironment_O::summaryOfContents() const {
  int tab = unbox_fixnum(gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabSTAR->symbolValue()));
  stringstream ss;
  this->_Macros->mapHash([tab, &ss](T_sp key, T_sp value) {
                ss << string(tab,' ') << _rep_(key);
                ss << " --> " << _rep_(value);
                ss << std::endl;
  });
  ss << this->Base::summaryOfContents();
  return ss.str();
}

CL_LISPIFY_NAME(makeStackValueEnvironment);
CL_DEFUN StackValueEnvironment_sp StackValueEnvironment_O::make(T_sp parent) {
  StackValueEnvironment_sp environ = StackValueEnvironment_O::create();
  environ->setupParent(parent);
  return environ;
}

bool StackValueEnvironment_O::_findValue(T_sp sym, int &depth, int &index, ValueKind &valueKind, T_sp &value) const {
  DEPRECIATED();
#if 0
	LOG(BF("Looking for binding for symbol(%s)") % _rep_(sym) );
	value = this->_Values->find(sym);
	if ( value.nilp() ) {
	    return this->Base::_findValue(sym,depth,index,valueKind,value);
	}
	LOG(BF(" Found binding %s")% fi->second );
	valueKind = stackValue;
#endif
  return true;
}

void StackValueEnvironment_O::addValue(T_sp sym, T_sp value) {
  this->_Values->hash_table_setf_gethash(sym, value);
}






void StackValueEnvironment_O::initialize() {
  this->Base::initialize();
  this->_Values = HashTableEq_O::create_default();
}

string StackValueEnvironment_O::summaryOfContents() const {
  int tab = unbox_fixnum(gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabSTAR->symbolValue()));
  stringstream ss;
  this->_Values->mapHash([tab, &ss](T_sp key, T_sp value) {
                ss << string(tab,' ') << _rep_(key);
                ss << " --> " << _rep_(value);
                ss << std::endl;
  });
  ss << this->Base::summaryOfContents();
  return ss.str();
}


void GlueEnvironment_O::initialize() {
  this->Base::initialize();
  this->_Map = HashTableEq_O::create_default();
}

#if 0
    T_mv GlueEnvironment_O::variable_lookup(Symbol_sp val) const
    {
	Cons_sp it = this->_Map->find(val);
	return(Values(oCdr(it),_lisp->_true()));
    }
#endif

}; // namespace core
