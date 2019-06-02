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

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
//#i n c l u d e "stringSet.h"
#include <clasp/core/symbolTable.h>
#include <clasp/core/environment.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/sequence.h>
#include <clasp/core/array.h>
#include <clasp/core/primitives.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/wrappers.h>

#define MAX_CONS_CHARS 1024

namespace core {

#define ENV_DEBUG_ON() (_sym_STARenvironment_debugSTAR->symbolValue().notnilp())


CL_LAMBDA(env sym);
CL_DECLARE();
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
    return _Nil<T_O>();
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
    ComplexVector_T_sp vo = ComplexVector_T_O::make(iEnd,_Nil<T_O>());
    for (int i(0); i < iEnd; ++i) {
      T_sp val = (*vf)[i];
      if (val.unboundp()) {
        val = _sym__BANG_unbound_BANG_;
      }
      vo->rowMajorAset(i, val);
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
  T_sp functionEnv = _Nil<T_O>();
  if (Environment_O::clasp_findFunction(env, name, depth, index, func, functionEnv)) {
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

bool clasp_updateValue(T_sp env, Symbol_sp sym, T_sp val) {
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


CL_DEFUN T_mv core__findValueEnvironmentAtDepth(T_sp env, int searchDepth) {
  int depth = 0;
  bool crossesFunction = false;
  T_sp found_env = _Nil<T_O>();
  Environment_O::clasp_findValueEnvironmentAtDepth(env,searchDepth,depth,crossesFunction,found_env);
  return Values(found_env,_lisp->_boolean(crossesFunction));
}

bool Environment_O::clasp_findValueEnvironmentAtDepth(T_sp env, int searchDepth, int& depth, bool& crossesFunction, T_sp& found_env) {
  if (env.nilp()) {
    return false;
  }
  return gc::As<Environment_sp>(env)->findValueEnvironmentAtDepth(searchDepth,depth,crossesFunction,found_env);
}

bool Environment_O::findValueEnvironmentAtDepth(int searchDepth, int& depth, bool& crossesFunction, T_sp& found_env) const {
  T_sp parent = this->getParentEnvironment();
  return clasp_findValueEnvironmentAtDepth(parent,searchDepth,depth,crossesFunction,found_env);
}

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
CL_LAMBDA(env &optional stop_at_function_container_environment);
CL_NAME(CURRENT-VISIBLE-ENVIRONMENT);
CL_DEFUN T_sp Environment_O::clasp_currentVisibleEnvironment(T_sp env, bool stopAtFunctionContainerEnvironment) {
  if (env.nilp())
    return (_Nil<T_O>());
  if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return (eenv->currentVisibleEnvironment(stopAtFunctionContainerEnvironment));
  }
  return env;
};

T_sp Environment_O::currentVisibleEnvironment(bool stopAtFunctionContainerEnvironment) const {
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
  stringstream sout;
  this->_environmentStackFill(1, sout);
  return sout.str();
}

void Environment_O::dump() {
  stringstream sout;
  this->_environmentStackFill(1, sout);
  printf("%s:%d Dumping environment\n%s\n", __FILE__, __LINE__, sout.str().c_str());
}

CL_DEFUN void core__environment_dump(T_sp env)
{
  if (env.nilp()) {
    printf("%s:%d environment is top-level\n", __FILE__, __LINE__ );
    return;
  }
  if ( Environment_sp e = env.asOrNull<Environment_O>() ) {
    e->dump();
    return;
  }
  SIMPLE_ERROR(BF("The argument %s was not an environment") % _rep_(env));
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

string Environment_O::__repr__() const {
  stringstream ss;
  ss << "#<(" << lisp_classNameAsString(cl__class_of(this->asSmartPtr())) << "@" << ((void*)this->asSmartPtr().raw_()) ;
#if 0
  int tab = gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabSTAR->symbolValue()).unsafe_fixnum();
  {
    ss << "-----------" << std::endl;
    tab += gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabIncrementSTAR->symbolValue()).unsafe_fixnum();
    Fixnum_sp fntab = make_fixnum(tab);
    DynamicScopeManager scope(_sym_STARenvironmentPrintingTabSTAR,fntab);
    ss <<this->summaryOfContents();
    if ( this->getParentEnvironment().notnilp() )
    {
      ss << string(tab,' ') << " :parent ";
      ss << _rep_(this->getParentEnvironment());
    }
    ss << string(tab,' ') << " )" << std::endl;
  }
#endif
  ss << ">";
  return ss.str();
}

bool Environment_O::_updateValue(Symbol_sp sym, T_sp obj) {
  if (this->getParentEnvironment().nilp()) {
    SIMPLE_ERROR(BF("Could not update local symbol(%s) because it was not defined") % _rep_(sym));
  }
  return clasp_updateValue(this->getParentEnvironment(), sym, obj);
}

bool Environment_O::findValue(T_sp sym, int &depth, int &index, bool& crossesFunction, ValueKind &valueKind, T_sp &value, T_sp& env) const {
  depth = 0;
  index = -1;
  crossesFunction = false;
  valueKind = undeterminedValue;
  env = _Unbound<T_O>();
  return this->_findValue(sym, depth, index, crossesFunction, valueKind, value, env);
}

bool Environment_O::clasp_findValue(T_sp env, T_sp sym, int &depth, int &index, bool& crossesFunction, ValueKind &valueKind, T_sp &value, T_sp& result_env) {
  if (env.nilp()) {
    depth = -1;
    index = -1;
    valueKind = undeterminedValue;
    return false;
  }
  ASSERT(gc::IsA<Environment_sp>(env));
  return gc::As_unsafe<Environment_sp>(env)->_findValue(sym, depth, index, crossesFunction, valueKind, value, result_env);
}

bool Environment_O::clasp_lexicalSpecialP(T_sp env, Symbol_sp sym) {
  if (env.nilp()) {
    return false;
  } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->lexicalSpecialP(sym);
  }
  NOT_ENVIRONMENT_ERROR(env);
}

bool Environment_O::_findValue(T_sp sym, int &depth, int &index, bool& crossesFunction, ValueKind &valueKind, T_sp &value, T_sp& env) const {
#ifdef DEBUG_ENVIRONMENTS
  if (ENV_DEBUG_ON()) printf("%s:%d:%s sym -> %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(sym).c_str());
#endif
  T_sp parent = clasp_currentVisibleEnvironment(this->getParentEnvironment(),false);
  return clasp_findValue(parent, sym, depth, index, crossesFunction, valueKind, value, env);
}

bool Environment_O::clasp_findFunction(T_sp env, T_sp functionName, int &depth, int &index, Function_sp &func, T_sp& functionEnv) {
  if (env.nilp()) {
    depth = -1;
    index = -1;
    return false;
  } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->_findFunction(functionName, depth, index, func, functionEnv);
  }
  NOT_ENVIRONMENT_ERROR(env);
}

bool Environment_O::_findFunction(T_sp functionName, int &depth, int &index, Function_sp &func, T_sp& functionEnv) const {
  return clasp_findFunction(this->getParentEnvironment(), functionName, depth, index, func, functionEnv);
}

bool Environment_O::findFunction(T_sp functionName, int &depth, int &index, Function_sp &value, T_sp& functionEnv) const {
  depth = 0;
  index = -1;
  functionEnv = _Nil<T_O>();
  return this->_findFunction(functionName, depth, index, value,functionEnv);
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

CL_DEFUN T_sp core__find_function_container_environment(T_sp env) {
  return Environment_O::clasp_find_current_code_environment(env);
}

T_sp Environment_O::clasp_find_current_code_environment(T_sp env) {
  if (env.nilp()) {
    SIMPLE_ERROR(BF("Could not find FunctionContainerEnvironment"));
  }
  return gc::As<Environment_sp>(env)->find_current_code_environment();
}
  
T_mv Environment_O::clasp_recognizesBlockSymbol(T_sp env, Symbol_sp sym, bool &interFunction) {
  if (env.nilp()) {
    return Values(_Nil<T_O>(), _Nil<T_O>(), _Nil<T_O>(), _Nil<T_O>());
  } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->recognizesBlockSymbol(sym, interFunction);
  }
  NOT_ENVIRONMENT_ERROR(env);
}

int Environment_O::clasp_getBlockSymbolFrame(T_sp env, Symbol_sp sym) {
  DEPRECATED();
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
  DEPRECATED();
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
#ifdef DEBUG_ENVIRONMENTS
  if (ENV_DEBUG_ON()) {
    printf("%s:%d:%s  sym is %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(sym).c_str() );
    printf("        Looking in environment: %s\n", _rep_(this->asSmartPtr()).c_str());
  }
#endif
  int depth = 0;
  int index = -1;
  bool crossesFunction = false;
  ValueKind valueKind = undeterminedValue;
  T_sp value;
  T_sp result_env = _Unbound<T_O>();;
  if (this->_findValue(sym, depth, index, crossesFunction, valueKind, value, result_env)) {
    switch (valueKind) {
    case lexicalValue:
        return Cons_O::createList(ext::_sym_lexicalVar, sym, make_fixnum(depth), make_fixnum(index), result_env, _lisp->_boolean(crossesFunction));
    case specialValue:
        return Cons_O::create(ext::_sym_specialVar, sym);
    case registerValue:
        return value;
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
  int depth = 0;
  int index = 0;
  bool interFunction = false;
  T_sp tagbodyEnv = _Nil<T_O>();
  if (this->_findTag(tag, depth, index, interFunction, tagbodyEnv)) {
    if (interFunction) {
      return Cons_O::createList(_sym_dynamicGo, make_fixnum(depth), make_fixnum(index), tagbodyEnv);
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
  T_sp functionEnv = _Nil<T_O>();
  if (this->findFunction(functionName, depth, index, value,functionEnv)) {
    return Cons_O::createList(_sym_lexicalFunction, functionName, make_fixnum(depth), make_fixnum(index), functionEnv);
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

CL_DEFUN size_t core__calculateRuntimeVisibleEnvironmentDepth(T_sp env, T_sp searchEnv ) {
  int depth = 0;
  if (Environment_O::clasp_calculateRuntimeVisibleEnvironmentDepth(env,searchEnv,depth)) {
    return depth;
  } else {
    SIMPLE_ERROR(BF("Could not find environment"));
  }
};

bool Environment_O::clasp_calculateRuntimeVisibleEnvironmentDepth(T_sp env, T_sp searchEnv, int& depth) {
  if (env.nilp()) {
    return false;
  }
  return gc::As<Environment_sp>(env)->calculateRuntimeVisibleEnvironmentDepth(searchEnv,depth);
}

bool Environment_O::calculateRuntimeVisibleEnvironmentDepth(T_sp searchEnv, int& depth) const {
  return clasp_calculateRuntimeVisibleEnvironmentDepth(this->getParentEnvironment(),searchEnv,depth);
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


bool Environment_O::clasp_findBlock(T_sp env, Symbol_sp sym, int &depth, bool &interFunction, T_sp &blockEnv) {
  if (env.nilp()) return false;
  if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    return eenv->_findBlock(sym, depth, interFunction, blockEnv);
  }
  NOT_ENVIRONMENT_ERROR(env);
}

bool Environment_O::_findBlock(Symbol_sp sym, int &depth, bool &interFunction, T_sp &blockEnv) const {
  return clasp_findBlock(this->getParentEnvironment(), sym, depth, interFunction, blockEnv);
}




CL_LISPIFY_NAME("countFunctionContainerEnvironments");
CL_DEFMETHOD int Environment_O::countFunctionContainerEnvironments() const {
  return clasp_countFunctionContainerEnvironments(this->getParentEnvironment());
}

CL_LISPIFY_NAME("find_block_named_environment");
CL_DEFMETHOD T_sp Environment_O::find_block_named_environment(Symbol_sp blockName) const {
  T_sp parent = this->getParentEnvironment();
  if (parent.nilp()) {
    SIMPLE_ERROR(BF("Could not find block with name[%s]") % _rep_(blockName));
  }
  return gc::As<Environment_sp>(parent)->find_block_named_environment(blockName);
}

CL_LISPIFY_NAME("find_unwindable_environment");
CL_DEFMETHOD T_sp Environment_O::find_unwindable_environment() const {
  return Environment_O::clasp_find_unwindable_environment(this->getParentEnvironment());
}

CL_LISPIFY_NAME("find_tagbody_tag_environment");
CL_DEFMETHOD T_sp Environment_O::find_tagbody_tag_environment(Symbol_sp tag) const {
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
  ASSERT(environ.nilp()||environ.asOrNull<Environment_O>());
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
  if (this->_Metadata->hashTableCount() > 0) {
    ss << string(tab, ' ') << "----Metadata follows ---" << std::endl;
    this->_Metadata->mapHash([tab, &ss](T_sp key, T_sp val) {
                    ss << string(tab,' ')<< _rep_(key) << " --> " << _rep_(val) << std::endl;
    });
    ss << string(tab, ' ') << "-----Metadata done ----" << std::endl;
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






RuntimeVisibleEnvironment_O::RuntimeVisibleEnvironment_O() : Base(), _Invisible(false) {};

T_sp RuntimeVisibleEnvironment_O::currentVisibleEnvironment(bool stopAtFunctionContainerEnvironment) const {
  if (this->_Invisible) {
    return clasp_currentVisibleEnvironment(this->getParentEnvironment(),stopAtFunctionContainerEnvironment);
  }
  return this->const_sharedThis<Environment_O>();
}

bool RuntimeVisibleEnvironment_O::calculateRuntimeVisibleEnvironmentDepth(T_sp searchEnv, int& depth) const {
  if (&*searchEnv == this) return true;
  if (!this->_Invisible) ++depth;
  return clasp_calculateRuntimeVisibleEnvironmentDepth(this->getParentEnvironment(),searchEnv,depth);
}

bool RuntimeVisibleEnvironment_O::_findTag(Symbol_sp sym, int &depth, int &index, bool &interFunction, T_sp &tagbodyEnv) const {
  T_sp parent = this->getParentEnvironment(); // clasp_currentVisibleEnvironment(this->getParentEnvironment());
  if (!this->_Invisible) ++depth;
  return clasp_findTag(parent, sym, depth, index, interFunction, tagbodyEnv);
}

bool RuntimeVisibleEnvironment_O::_findBlock(Symbol_sp sym, int &depth, bool &interFunction, T_sp &blockEnv) const {
  T_sp parent = this->getParentEnvironment(); // clasp_currentVisibleEnvironment(this->getParentEnvironment());
  if (!this->_Invisible) ++depth;
  return clasp_findBlock(parent, sym, depth, interFunction, blockEnv);
}

bool RuntimeVisibleEnvironment_O::_findValue(T_sp sym, int &depth, int &index, bool& crossesFunction, ValueKind &valueKind, T_sp &value, T_sp& env) const {
#ifdef DEBUG_ENVIRONMENTS
  if (ENV_DEBUG_ON()) printf("%s:%d:%s sym -> %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(sym).c_str());
#endif
  T_sp parent = this->getParentEnvironment(); // clasp_currentVisibleEnvironment(this->getParentEnvironment());
  if (!this->_Invisible) ++depth;
  return clasp_findValue(parent, sym, depth, index, crossesFunction, valueKind, value, env);
}

bool RuntimeVisibleEnvironment_O::_findFunction(T_sp functionName, int &depth, int &index, Function_sp &func, T_sp& functionEnv) const {
  T_sp parent = clasp_currentVisibleEnvironment(this->getParentEnvironment(),false);
  LOG(BF("Moving down a level"));
  if (!this->_Invisible) ++depth;
  return clasp_findFunction(parent, functionName, depth, index, func, functionEnv);
}

bool RuntimeVisibleEnvironment_O::findValueEnvironmentAtDepth(int searchDepth, int &depth, bool& crossesFunction, T_sp& env) const {
  if (!this->_Invisible) {++depth;}
  T_sp parent = this->getParentEnvironment();
  return Environment_O::clasp_findValueEnvironmentAtDepth(parent,searchDepth,depth,crossesFunction,env);
}





void ValueEnvironment_O::initialize() {
  this->Base::initialize();
}

List_sp ValueEnvironment_O::find(Symbol_sp sym) const {
  if (this->_SymbolIndex_alist.consp()) {
    for ( auto cur : this->_SymbolIndex_alist ) {
      T_sp pair = CONS_CAR(cur);
      if (pair.consp()) {
        if (CONS_CAR(pair) == sym) return pair;
      } else {
        not_alist_error(this->_SymbolIndex_alist);
      }
    }
  }
  return _Nil<T_O>();
}

void ValueEnvironment_O::augment(Symbol_sp sym, T_sp value) {
  Cons_sp pair = Cons_O::create(sym,value);
  this->_SymbolIndex_alist = Cons_O::create(pair,this->_SymbolIndex_alist);
}

bool ValueEnvironment_O::lexicalSpecialP(Symbol_sp sym) const {
  // Lookup the symbol in our list Symbol map
  List_sp fi = this->find(sym);
  if (fi.consp()) {
  // If the target index is a SPECIAL_TARGET then return true otherwise false
    T_sp entry = CONS_CDR(fi);
    return (entry.fixnump()&&entry.unsafe_fixnum() == SPECIAL_TARGET);
  }    
    // if we don't find it then invoke Environment_O::lexicalSpecialP
  return this->Base::lexicalSpecialP(sym);
}

T_sp ValueEnvironment_O::getActivationFrame() const {
  return this->_ActivationFrame;
}


CL_LISPIFY_NAME("valueEnvironment_defineLexicalBinding");
CL_DEFMETHOD void ValueEnvironment_O::defineLexicalBinding(Symbol_sp sym, int idx) {
  this->augment(sym, make_fixnum(idx));
}

CL_LISPIFY_NAME("valueEnvironment_defineSpecialBinding");
CL_DEFMETHOD void ValueEnvironment_O::defineSpecialBinding(Symbol_sp sym) {
  this->augment(sym, make_fixnum(SPECIAL_TARGET));
}


bool ValueEnvironment_O::findValueEnvironmentAtDepth(int searchDepth, int &depth, bool& crossesFunction, T_sp& env) const {
  if (searchDepth == depth) {
    env = this->asSmartPtr();
    return true;
  }
  return this->Base::findValueEnvironmentAtDepth(searchDepth,depth,crossesFunction,env);
}


bool ValueEnvironment_O::_findValue(T_sp sym, int &depth, int &index, bool& crossesFunction, ValueKind &valueKind, T_sp &value, T_sp& env) const {
  LOG(BF("Looking for binding for symbol(%s)") % _rep_(sym));
  //    LOG(BF("The frame stack is %d deep") % this->depth() );
  
  List_sp fi = this->find(sym);
#ifdef DEBUG_ENVIRONMENTS
  if (ENV_DEBUG_ON()) {
    printf("%s:%d  In ValueEnvironment_O::_findValue sym %s   find -> %s\n", __FILE__, __LINE__, _rep_(sym).c_str(), _rep_(fi).c_str());
    printf("        Looking in environment: %s\n", _rep_(this->asSmartPtr()).c_str());
  }
#endif
  if (fi.consp()) {
    T_sp entry = CONS_CDR(fi);
    if (entry.fixnump()) {
      index = entry.unsafe_fixnum();
      if (index == SPECIAL_TARGET) {
        valueKind = specialValue;
        return true; // This was returning false for special values
      }
      valueKind = lexicalValue;
      env = this->asSmartPtr();
      LOG(BF(" Found binding %s") % _rep_(entry));
      ValueFrame_sp vframe = this->_ActivationFrame;
      value = vframe->entry(index);
      return true;
    } else if (entry.consp()) {
      T_sp entryKind = CONS_CAR(entry);
      if (entryKind == ext::_sym_registerVar) {
        valueKind = registerValue;
        value = entry;
        return true;
      } else {
        SIMPLE_ERROR(BF("Handle ValueEnvironment_O entryKind of %s in %s") % _rep_(entryKind) % this->summaryOfContents());
      }
    } else {
      SIMPLE_ERROR(BF("Handle ValueEnvironment_O entry of %s in %s") % _rep_(fi) % this->summaryOfContents());
    }
  }
#ifdef DEBUG_ENVIRONMENTS
  if (ENV_DEBUG_ON()) {
    printf("%s:%d  Continuing search for %s\n", __FILE__, __LINE__, _rep_(sym).c_str() );
  }
#endif
  return this->Base::_findValue(sym, depth, index, crossesFunction, valueKind, value, env );
}

bool ValueEnvironment_O::_findSymbolMacro(Symbol_sp sym, int &depth, int &index, bool &shadowed, Function_sp &fn) const {
  LOG(BF("Looking for binding for symbol(%s)") % _rep_(sym));
  //    LOG(BF("The frame stack is %d deep") % this->depth() );
  List_sp fi = this->find(sym);
  if (fi.consp()) {
    T_sp entry = CONS_CDR(fi);
    if (entry.fixnump()) {
      index = entry.unsafe_fixnum();
      shadowed = true;
      return false;
    } else if (entry.consp()) {
      shadowed = true;
      return false;
    }
  }
  return this->Base::_findSymbolMacro(sym, depth, index, shadowed, fn);
}

bool ValueEnvironment_O::activationFrameElementBoundP(int idx) const {
  ValueFrame_sp vframe = this->_ActivationFrame;
  return vframe->boundp_entry(idx);
}

CL_LISPIFY_NAME(makeValueEnvironment);
CL_DEFUN ValueEnvironment_sp ValueEnvironment_O::createForLambdaListHandler(LambdaListHandler_sp llh, T_sp parent) {
  ValueEnvironment_sp env(ValueEnvironment_O::create());
  env->setupForLambdaListHandler(llh, parent);
  return env;
}

CL_LISPIFY_NAME(makeValueEnvironmentForNumberOfEntries);
CL_LAMBDA(num-args parent &optional invisible);
CL_DEFUN ValueEnvironment_sp ValueEnvironment_O::createForNumberOfEntries(int numberOfArguments, T_sp parent, bool invisible) {
  ValueEnvironment_sp env(ValueEnvironment_O::create());
  env->_Invisible = invisible;
  env->setupParent(parent);
  if (!invisible) env->_ActivationFrame = ValueFrame_O::create(numberOfArguments, clasp_getActivationFrame(clasp_currentVisibleEnvironment(parent,false)));
  else env->_ActivationFrame = gc::As<ValueFrame_sp>(clasp_getActivationFrame(clasp_currentVisibleEnvironment(parent,false)));
  return env;
}

CL_LISPIFY_NAME(makeValueEnvironmentForLocallySpecialEntries);
CL_DEFUN ValueEnvironment_sp ValueEnvironment_O::createForLocallySpecialEntries(List_sp specials, T_sp parent) {
  LOG(BF("specials -> %s parent -> %s\n") % _rep_(specials) % _rep_(parent));
  ValueEnvironment_sp env(ValueEnvironment_O::create());
  env->setupParent(parent);
  env->_Invisible = true; // What do we do with the activation frame?
  env->_ActivationFrame = _Unbound<ValueFrame_O>();
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
  this->_ActivationFrame = ValueFrame_O::create(numberOfLexicals, clasp_getActivationFrame(clasp_currentVisibleEnvironment(parent,false)));
}

string ValueEnvironment_O::summaryOfContents() const {
  int tab = unbox_fixnum(gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabSTAR->symbolValue()));
  stringstream ss;
  ss << string(tab, ' ') << "ValueEnvironment_O values @" << (void*)this;
  if (this->_Invisible) { ss << string(tab,' ') << " invisible "; };
  ss << std::endl;
  ValueFrame_sp vframe = this->_ActivationFrame;
  int numEntries = 0;
  for ( auto cur : this->_SymbolIndex_alist ) {
    T_sp entry = CONS_CAR(cur);
    if (entry.consp()) {
      ++numEntries;
      T_sp key = CONS_CAR(entry);
      T_sp value = CONS_CDR(entry);
      if (value.fixnump()) {
        int ivalue = unbox_fixnum(gc::As<Fixnum_sp>(value));
        ss << string(tab,' ') << _rep_(entry) << " -> ";
        if ( ivalue == SPECIAL_TARGET ) {
          ss << "SPECIAL-VAR" << std::endl;
        } else if (!vframe.unboundp()) {
          if ( ivalue >= cl__length(vframe) ) {
            ss << "ActivationFrame->index["<<ivalue<<"]->OUT-OF-RANGE";
          } else if ( vframe->boundp_entry(ivalue) ) {
            ss << _rep_(vframe->entry(ivalue));
          } else {
            ss << "UNBOUND ";
          }
          ss << std::endl;
        } else {
          ss << "NO_ACTIVATION_FRAME" << std::endl;
        }
      } else if (value.consp()) {
        ss << "( " << _rep_(key) << " . " << _rep_(value) << " )" << std::endl;
      }
    } else {
      SIMPLE_ERROR(BF("A ValueEnvironment is not an alist"));
    }
  }
  ss << string(tab,' ') << "Number of entries " << numEntries << std::endl;
  ss << this->Base::summaryOfContents();
  return ss.str();
}


List_sp ValueEnvironment_O::allLocalNamesAsCons() const {
  List_sp result = _Nil<T_O>();
  for ( auto cur : this->_SymbolIndex_alist ) {
    T_sp entry = CONS_CAR(cur);
    if (entry.consp()) {
      T_sp key = CONS_CAR(entry);
      T_sp val = CONS_CDR(entry);
      if (!(val.fixnump() && val.unsafe_fixnum() == SPECIAL_TARGET )) {
        result = Cons_O::create(Cons_O::create(key,val), result);
      }
    } else {
      SIMPLE_ERROR(BF("The ValueEnvironment is not an alist!!"));
    }
  }
  return result;
}

CL_DEFUN List_sp core__ValueEnvironment_nonSpecialSymbols(ValueEnvironment_sp env) {
  return env->allLocalNamesAsCons();
}
        
/*! If the symbol is not in the lexical environment then throw an exception.
      If the symbol is lexical and was updated return true.
      If the symbol is locally special then don't update it (caller is responsible for doing that) and return false.
    */
bool ValueEnvironment_O::_updateValue(Symbol_sp sym, T_sp obj) {
  List_sp it = this->find(sym);
  if (it.nilp()) {
    T_sp parent = this->getParentEnvironment();
    if (parent.nilp()) {
      SIMPLE_ERROR(BF("Could not update local symbol(%s) because it was not defined") % _rep_(sym));
    }
    return clasp_updateValue(clasp_currentVisibleEnvironment(parent,false), sym, obj);
  }
#if 0 // def DEBUG_ASSERT
  if ( sym->symbolNameAsString()== "ENV") {
    if ( !(obj.nilp() || obj.asOrNull<Environment_O>()) ) {
      printf("%s:%d ValueEnvironment_O@%p::_updateValue to lexical ENV --> %s\n",
             __FILE__, __LINE__, (void*)this, _rep_(obj).c_str() );
      printf("%s:%d     The value isn't an environment - trap this\n", __FILE__, __LINE__ );
    }
  }
#endif
  int ivalue = unbox_fixnum(gc::As<Fixnum_sp>(oCdr(it)));
  if (ivalue < 0) {
    //	    sym->setf_symbolValue(obj);
    return false;
  }
  ValueFrame_sp vframe = this->_ActivationFrame;
  vframe->set_entry(ivalue, obj);
  return true;
}

T_sp ValueEnvironment_O::new_binding(Symbol_sp sym, int idx, T_sp obj) {
  ASSERT(idx>=0);
#if 0
  if (this->find(sym).notnilp()) {
    SIMPLE_ERROR(BF("The symbol[%s] is already in the environment") % _rep_(sym));
  }
#endif
#if 0 // def DEBUG_ASSERT
  if ( sym->symbolNameAsString()== "ENV") {
    if ( !(obj.nilp() || obj.asOrNull<Environment_O>()) ) {
      printf("%s:%d ValueEnvironment_O@%p::new_binding lexical ENV --> %s\n",
             __FILE__, __LINE__, (void*)this, _rep_(obj).c_str() );
      printf("%s:%d     The value isn't an environment - trap this\n", __FILE__, __LINE__ );
    }
  }
#endif
  this->_SymbolIndex_alist = Cons_O::create(Cons_O::create(sym,make_fixnum(idx)),this->_SymbolIndex_alist);
  ValueFrame_sp vframe = this->_ActivationFrame;
  vframe->set_entry(idx, obj);
  return obj;
}




T_sp FunctionValueEnvironment_O::getActivationFrame() const {
  return this->_FunctionFrame;
};

bool FunctionValueEnvironment_O::_findFunction(T_sp functionName, int &depth, int &index, Function_sp &value, T_sp& functionEnv) const {
  LOG(BF("Looking for binding for function name[%s]") % _rep_(functionName));
  //    LOG(BF("The frame stack is %d deep") % this->depth() );
  T_mv mv = this->_FunctionIndices->gethash(functionName, _Nil<T_O>());
  T_sp val = mv;
  bool foundp = mv.valueGet_(1).isTrue();
  if (!foundp)
    return this->Base::_findFunction(functionName, depth, index, value, functionEnv );
  index = unbox_fixnum(gc::As<Fixnum_sp>(val));
  functionEnv = this->asSmartPtr();
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
  environ->_FunctionFrame = FunctionFrame_O::create(numEntries, clasp_getActivationFrame(clasp_currentVisibleEnvironment(parent,false)));
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
      ss << "function " << _rep_(func->functionName());
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
  return clasp_getActivationFrame(this->currentVisibleEnvironment(false));
};

T_sp CompileTimeEnvironment_O::currentVisibleEnvironment(bool stopAtFunctionContainerEnvironment) const {
  T_sp parent = this->getParentEnvironment();
  if (parent.nilp()) return _Nil<T_O>();
  return clasp_currentVisibleEnvironment(parent,stopAtFunctionContainerEnvironment);
}

bool CompileTimeEnvironment_O::_findValue(T_sp sym, int &depth, int &index, bool& crossesFunction, ValueKind &valueKind, T_sp &value, T_sp& env) const {
  T_sp parent = clasp_currentVisibleEnvironment(this->getParentEnvironment(),false);
  return clasp_findValue(parent, sym, depth, index, crossesFunction, valueKind, value, env);
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
  environ->_Invisible = false;
  if (!environ->_Invisible) environ->_ActivationFrame = ValueFrame_O::create(1,clasp_getActivationFrame(clasp_currentVisibleEnvironment(parent,false)));
  else environ->_ActivationFrame = gc::As<ValueFrame_sp>(clasp_getActivationFrame(clasp_currentVisibleEnvironment(parent,false)));
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

bool BlockEnvironment_O::_findBlock(Symbol_sp sym, int &depth, bool &interFunction, T_sp &blockEnv) const {
  //	printf("%s:%d searched through BlockEnvironment_O\n", __FILE__, __LINE__ );
  if (!this->_Invisible) {
    if (sym == this->_BlockSymbol) {
      blockEnv = this->asSmartPtr();
      return true;
    }
    if (this->getParentEnvironment().nilp()) {
      return false;
    }
    ++depth;
  }
  return clasp_findBlock(this->getParentEnvironment(), sym, depth, interFunction, blockEnv);
}

T_sp BlockEnvironment_O::getActivationFrame() const {
  return this->_ActivationFrame;
}

T_mv BlockEnvironment_O::recognizesBlockSymbol(Symbol_sp sym, bool &interFunction) const {
  if (this->_BlockSymbol == sym)
    return Values(_lisp->_true(), _lisp->_boolean(interFunction), this->asSmartPtr());
  return clasp_recognizesBlockSymbol(this->getParentEnvironment(), sym, interFunction);
}

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
T_sp FunctionContainerEnvironment_O::currentVisibleEnvironment(bool stopAtFunctionContainerEnvironment) const {
  if (stopAtFunctionContainerEnvironment) return this->asSmartPtr();
  T_sp parent = this->getParentEnvironment();
  if (parent.nilp()) return _Nil<T_O>();
  return clasp_currentVisibleEnvironment(parent,stopAtFunctionContainerEnvironment);
}

CL_LISPIFY_NAME(makeFunctionContainerEnvironment);
CL_DEFUN FunctionContainerEnvironment_sp FunctionContainerEnvironment_O::make(T_sp parent,T_sp closure,T_sp function) {
  FunctionContainerEnvironment_sp environ = FunctionContainerEnvironment_O::create(parent);
  environ->_Closure = closure;
  environ->_Function = function;
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

bool FunctionContainerEnvironment_O::_findBlock(Symbol_sp sym, int &depth, bool &interFunction, T_sp &blockEnv) const {
  // We are crossing a function boundary - set interFunction to true
  //	printf("%s:%d searched through FunctionContainerEnvironment_O\n", __FILE__, __LINE__ );
  interFunction = true;
  if (this->getParentEnvironment().nilp()) {
    return false;
  }
  return clasp_findBlock(this->getParentEnvironment(), sym, depth, interFunction, blockEnv);
}

bool FunctionContainerEnvironment_O::_findValue(T_sp sym, int &depth, int &index, bool& crossesFunction, ValueKind &valueKind, T_sp &value, T_sp& env) const {
  // We are crossing a function boundary - set interFunction to true
  //	printf("%s:%d searched through FunctionContainerEnvironment_O\n", __FILE__, __LINE__ );
  crossesFunction = true;
  if (this->getParentEnvironment().nilp()) {
    return false;
  }
  return clasp_findValue(this->getParentEnvironment(), sym, depth, index, crossesFunction, valueKind, value, env);
}

bool FunctionContainerEnvironment_O::findValueEnvironmentAtDepth(int searchDepth, int &depth, bool& crossesFunction, T_sp& env) const {
  // We are crossing a function boundary - set interFunction to true
  //	printf("%s:%d searched through FunctionContainerEnvironment_O\n", __FILE__, __LINE__ );
  crossesFunction = true;
  if (this->getParentEnvironment().nilp()) {
    return false;
  }
  return clasp_findValueEnvironmentAtDepth(this->getParentEnvironment(), searchDepth, depth, crossesFunction, env );
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
  environ->_Invisible = false;
  environ->_ActivationFrame = ValueFrame_O::create(1,clasp_getActivationFrame(clasp_currentVisibleEnvironment(parent,false)));
  return environ;
}



string TagbodyEnvironment_O::summaryOfContents() const {
  int tab = unbox_fixnum(gc::As<Fixnum_sp>(_sym_STARenvironmentPrintingTabSTAR->symbolValue()));
  stringstream ss;
  ValueFrame_sp vf = gc::As<ValueFrame_sp>(this->getActivationFrame());
    ss << ":tagbody-id " << (void*)vf->operator[](0).raw_() << std::endl;
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
  DEPRECATED();
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
  ql::list args;
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
  LOG(BF(" Found binding %s") % _rep_(fi));
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
  LOG(BF(" Found binding %s") % _rep_(fi));
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

void GlueEnvironment_O::initialize() {
  this->Base::initialize();
  this->_Map = HashTableEq_O::create_default();
}

}; // namespace core
