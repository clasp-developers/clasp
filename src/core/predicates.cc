/*
    File: predicates.cc
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
#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lispList.h>
#include <clasp/core/metaClass.h>
#include <clasp/core/package.h>
#include <clasp/core/bignum.h>
#include <clasp/core/closPackage.h>
#include <clasp/core/bitVector.h>
#include <clasp/core/pathname.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/random.h>
#include <clasp/core/vectorObjects.h>
//#ifndef CLOS
#include <clasp/core/structureObject.h>
//#else
#include <clasp/core/instance.h>
//#endif
#include <clasp/core/readtable.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/numbers.h>
#include <clasp/core/externalObject.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/str.h>
#include <clasp/core/wrappers.h>

namespace core {

#define ARGS_core_baseCharP "(arg)"
#define DECL_core_baseCharP ""
#define DOCS_core_baseCharP "baseCharP"
bool core_baseCharP(T_sp arg) {
  if (Character_sp c = arg.asOrNull<Character_O>()) {
    (void)c;
    return true;
  }
  return false;
};

#define ARGS_af_bignumP "(arg)"
#define DECL_af_bignumP ""
#define DOCS_af_bignumP "bignumP"
bool af_bignumP(T_sp obj) {
  return gc::IsA<Bignum_sp>(obj);
};

#define ARGS_af_fixnumP "(arg)"
#define DECL_af_fixnumP ""
#define DOCS_af_fixnumP "fixnumP"
bool af_fixnumP(T_sp obj) {
  return obj.fixnump();
}

#define ARGS_af_stringP "(arg)"
#define DECL_af_stringP ""
#define DOCS_af_stringP "stringP"
bool af_stringP(T_sp obj) {
  return gc::IsA<String_sp>(obj);
};

#define ARGS_af_strP "(arg)"
#define DECL_af_strP ""
#define DOCS_af_strP "strP"
bool af_strP(T_sp obj) {
  return gc::IsA<Str_sp>(obj);
};

#define ARGS_af_doubleFloatP "(arg)"
#define DECL_af_doubleFloatP ""
#define DOCS_af_doubleFloatP "doubleFloatP"
bool af_doubleFloatP(T_sp obj) {
  return gc::IsA<DoubleFloat_sp>(obj);
};

#define ARGS_cl_functionp "(arg)"
#define DECL_cl_functionp ""
#define DOCS_cl_functionp "functionP"
bool cl_functionp(T_sp obj) {
  if (Instance_sp inst_obj = obj.asOrNull<Instance_O>()) {
    return inst_obj->isgf();
  }
  return gc::IsA<Function_sp>(obj);
};

#define ARGS_af_interpretedFunctionP "(arg)"
#define DECL_af_interpretedFunctionP ""
#define DOCS_af_interpretedFunctionP "interpretedFunctionP"
bool af_interpretedFunctionP(T_sp arg) {
  if (gc::IsA<Function_sp>(arg)) {
    if (auto intfunc = gc::As<Function_sp>(arg)->closure.asOrNull<InterpretedClosure>()) {
      (void)intfunc;
      return true;
    }
  }
  return false;
};

#define ARGS_cl_packagep "(arg)"
#define DECL_cl_packagep ""
#define DOCS_cl_packagep "See CLHS packagep"
bool cl_packagep(T_sp obj) {
  return gc::IsA<Package_sp>(obj);
};

#define ARGS_af_classp "(arg)"
#define DECL_af_classp ""
#define DOCS_af_classp "classp"
bool af_classp(T_sp obj) {
  return gc::IsA<Class_sp>(obj);
};

#define ARGS_core_builtInClassP "(arg)"
#define DECL_core_builtInClassP ""
#define DOCS_core_builtInClassP "classp"
bool core_builtInClassP(T_sp obj) {
  if (!af_classp(obj))
    return false;
  Class_sp c = gc::As<Class_sp>(obj);
  return c->builtInClassP();
};

#define ARGS_core_cxxInstanceP "(arg)"
#define DECL_core_cxxInstanceP ""
#define DOCS_core_cxxInstanceP "classp"
bool core_cxxInstanceP(T_sp obj) {
  if (Instance_sp ip = obj.asOrNull<Instance_O>()) {
    (void)ip;
    return true;
  }
  return false;
};

#define ARGS_af_lambda_list_handler_p "(arg)"
#define DECL_af_lambda_list_handler_p ""
#define DOCS_af_lambda_list_handler_p "lambda_list_handler_p"
bool af_lambda_list_handler_p(T_sp obj) {
  return gc::IsA<LambdaListHandler_sp>(obj);
};

#define ARGS_cl_numberp "(arg)"
#define DECL_cl_numberp ""
#define DOCS_cl_numberp "numberP"
bool cl_numberp(T_sp obj) {
  _G();
  return gc::IsA<Number_sp>(obj);
};

#define ARGS_af_complexP "(arg)"
#define DECL_af_complexP ""
#define DOCS_af_complexP "complexP"
bool af_complexP(T_sp obj) {
  _G();
  return gc::IsA<Complex_sp>(obj);
};

#define ARGS_af_ratioP "(arg)"
#define DECL_af_ratioP ""
#define DOCS_af_ratioP "ratioP"
bool af_ratioP(T_sp obj) {
  _G();
  return gc::IsA<Ratio_sp>(obj);
};

#define ARGS_cl_random_state_p "(arg)"
#define DECL_cl_random_state_p ""
#define DOCS_cl_random_state_p "ratioP"
bool cl_random_state_p(T_sp obj) {
  return gc::IsA<RandomState_sp>(obj);
};

#define ARGS_af_rationalP "(arg)"
#define DECL_af_rationalP ""
#define DOCS_af_rationalP "rationalP"
bool af_rationalP(T_sp obj) {
  _G();
  return gc::IsA<Rational_sp>(obj);
};

#define ARGS_af_longFloatP "(arg)"
#define DECL_af_longFloatP ""
#define DOCS_af_longFloatP "longFloatP"
bool af_longFloatP(T_sp obj) {
  return gc::IsA<LongFloat_sp>(obj);
};

#define ARGS_af_shortFloatP "(arg)"
#define DECL_af_shortFloatP ""
#define DOCS_af_shortFloatP "shortFloatP"
bool af_shortFloatP(T_sp obj) {
  _G();
  return gc::IsA<ShortFloat_sp>(obj);
};

#define ARGS_af_singleFloatP "(arg)"
#define DECL_af_singleFloatP ""
#define DOCS_af_singleFloatP "singleFloatP"
bool af_singleFloatP(T_sp obj) {
  _G();
  return gc::IsA<SingleFloat_sp>(obj);
};

#define ARGS_af_realP "(arg)"
#define DECL_af_realP ""
#define DOCS_af_realP "realP"
bool af_realP(T_sp obj) {
  _G();
  return gc::IsA<Real_sp>(obj);
};

#define ARGS_af_floatP "(arg)"
#define DECL_af_floatP ""
#define DOCS_af_floatP "floatP"
bool af_floatP(T_sp obj) {
  return gc::IsA<Float_sp>(obj);
};

#define ARGS_af_characterP "(arg)"
#define DECL_af_characterP ""
#define DOCS_af_characterP "characterP"
bool af_characterP(T_sp obj) {
  return obj.characterp();
};

#define ARGS_af_vectorP "(arg)"
#define DECL_af_vectorP ""
#define DOCS_af_vectorP "vectorP"
bool af_vectorP(T_sp obj) {
  return gc::IsA<Vector_sp>(obj);
};

#define ARGS_af_integerP "(arg)"
#define DECL_af_integerP ""
#define DOCS_af_integerP "integerP"
bool af_integerP(T_sp obj) {
  return gc::IsA<Integer_sp>(obj);
};

#define ARGS_af_keywordP "(arg)"
#define DECL_af_keywordP ""
#define DOCS_af_keywordP "keywordP"
bool af_keywordP(T_sp obj) {
  if (Symbol_sp s = obj.asOrNull<Symbol_O>()) {
    return s->isKeywordSymbol();
  }
  return false;
};

#define ARGS_af_pathP "(arg)"
#define DECL_af_pathP ""
#define DOCS_af_pathP "pathP"
bool af_pathP(T_sp obj) {
  return gc::IsA<Path_sp>(obj);
};

#define ARGS_af_bitVectorP "(arg)"
#define DECL_af_bitVectorP ""
#define DOCS_af_bitVectorP "bitVectorP"
bool af_bitVectorP(T_sp obj) {
  return gc::IsA<BitVector_sp>(obj);
};

#define ARGS_af_hashTableP "(arg)"
#define DECL_af_hashTableP ""
#define DOCS_af_hashTableP "hashTableP"
bool af_hashTableP(T_sp obj) {
  if (HashTable_sp ht = obj.asOrNull<HashTable_O>()) {
    (void)ht;
    return true;
  }
  return false;
};

#define ARGS_cl_readtablep "(arg)"
#define DECL_cl_readtablep ""
#define DOCS_cl_readtablep "readtablep"
bool cl_readtablep(T_sp obj) {
  return gc::IsA<ReadTable_sp>(obj);
};

#define ARGS_af_structureObjectP "(arg)"
#define DECL_af_structureObjectP ""
#define DOCS_af_structureObjectP "structureObjectP"
bool af_structureObjectP(T_sp obj) {
  if (gc::IsA<StructureObject_sp>(obj))
    return true;
  return gc::IsA<Instance_sp>(obj);
};

#define ARGS_af_arrayP "(arg)"
#define DECL_af_arrayP ""
#define DOCS_af_arrayP "arrayP"
bool af_arrayP(T_sp obj) {
  return gc::IsA<Array_sp>(obj);
};

#define ARGS_af_singleDispatchGenericFunctionP "(arg)"
#define DECL_af_singleDispatchGenericFunctionP ""
#define DOCS_af_singleDispatchGenericFunctionP "singleDispatchGenericFunctionP"
bool af_singleDispatchGenericFunctionP(T_sp obj) {
  return gc::IsA<SingleDispatchGenericFunction_sp>(obj);
};

#define ARGS_af_activation_frame_p "(arg)"
#define DECL_af_activation_frame_p ""
#define DOCS_af_activation_frame_p "activation_frame_p"
bool af_activation_frame_p(T_sp obj) {
  return gc::IsA<ActivationFrame_sp>(obj);
};

#define ARGS_af_externalObjectP "(arg)"
#define DECL_af_externalObjectP ""
#define DOCS_af_externalObjectP "externalObjectP"
bool af_externalObjectP(T_sp obj) {
  _G();
  return gc::IsA<ExternalObject_sp>(obj);
};

#define ARGS_af_simple_bit_vector_p "(arg)"
#define DECL_af_simple_bit_vector_p ""
#define DOCS_af_simple_bit_vector_p "simple_bit_vector_p"
bool af_simple_bit_vector_p(T_sp o) {
  if (SimpleBitVector_sp sbv = o.asOrNull<SimpleBitVector_O>()) {
    (void)sbv;
    return true;
  }
  return false;
};

#define ARGS_af_simple_vector_p "(arg)"
#define DECL_af_simple_vector_p ""
#define DOCS_af_simple_vector_p "simple_vector_p"
bool af_simple_vector_p(T_sp o) {
  if (VectorObjects_sp vo = o.asOrNull<VectorObjects_O>()) {
    (void)vo;
    return true;
  }
  return false;
};

#define ARGS_af_compiled_function_p "(arg)"
#define DECL_af_compiled_function_p ""
#define DOCS_af_compiled_function_p "compiled_function_p"
bool af_compiled_function_p(T_sp o) {
  if (Function_sp fn = o.asOrNull<Function_O>()) {
    (void)fn;
    return fn->closure->compiledP();
  }
  return false;
};

#define ARGS_af_genericFunctionP "(arg)"
#define DECL_af_genericFunctionP ""
#define DOCS_af_genericFunctionP "genericFunctionP"
bool af_genericFunctionP(T_sp o) {
  if (Function_sp cf = o.asOrNull<Function_O>()) {
    (void)cf;
    IMPLEMENT_MEF(BF("I should have a more sophisticated test here"));
    return true;
  }
  return false;
};

#define ARGS_af_properListP "(arg)"
#define DECL_af_properListP ""
#define DOCS_af_properListP "Return true if arg is a proper list"
bool af_properListP(T_sp arg) {
  T_sp fast, slow;
  bool test = true;
  fast = slow = arg;
  for (int n = 0; !fast.nilp(); n++, fast = oCdr(fast)) {
    if (!cl_listp(fast)) {
      test = false;
      break;
    }
    if (n & 1) {
      /* Circular list? */
      if (slow == fast) {
        test = false;
        break;
      }
      slow = oCdr(slow);
    }
  }
  return test;
};

#define ARGS_af_pathnamep "(arg)"
#define DECL_af_pathnamep ""
#define DOCS_af_pathnamep "pathnamep"
bool af_pathnamep(T_sp obj) {
  return gc::IsA<Pathname_sp>(obj);
};

#define ARGS_af_logicalPathnameP "(arg)"
#define DECL_af_logicalPathnameP ""
#define DOCS_af_logicalPathnameP "logicalPathnameP"
bool af_logicalPathnameP(T_sp obj) {
  return gc::IsA<LogicalPathname_sp>(obj);
};

void initialize_predicates() {
  af_def(ClPkg, "endp", &cl_endp);
#define newNameDefun(pkg, myname, lispname) af_def(pkg, #lispname, &myname, ARGS_##myname, DECL_##myname, DOCS_##myname)
  af_def(ClPkg, "symbolp", &cl_symbolp);
  af_def(ClPkg, "consp", &cl_consp);
  af_def(ClPkg, "listp", &cl_listp);
  ClDefun(numberp);
// clang-format off
  newNameDefun(CorePkg, af_integerP, cl:integerp);
  newNameDefun(CorePkg, af_rationalP, cl:rationalp);
  newNameDefun(CorePkg, af_floatP, cl:floatp);
  newNameDefun(CorePkg, af_realP, cl:realp);
  newNameDefun(CorePkg, af_complexP, cl:complexp);
  newNameDefun(CorePkg, af_characterP, cl:characterp);
  newNameDefun(CorePkg, af_stringP, cl:stringp);
  newNameDefun(CorePkg, af_bitVectorP, cl:bit_vector_p);
  newNameDefun(CorePkg, af_vectorP, cl:vectorp);
  newNameDefun(CorePkg, af_simple_vector_p, cl:simple_vector_p);
  newNameDefun(CorePkg, af_strP, cl:simple_string_p);
  newNameDefun(CorePkg, af_simple_bit_vector_p, cl:simple_bit_vector_p);
  newNameDefun(CorePkg, af_arrayP, cl:arrayp);
  newNameDefun(CorePkg, cl_packagep, cl:packagep);
  newNameDefun(CorePkg, cl_functionp, cl:functionp);
  newNameDefun(CorePkg, af_compiled_function_p, cl:compiled_function_p);
  newNameDefun(CorePkg, af_genericFunctionP, core:genericFunctionP);
  newNameDefun(CorePkg, af_keywordP, cl:keywordp);
  newNameDefun(CorePkg, cl_atom, cl:atom);
  newNameDefun(CorePkg, af_fixnumP, core:fixnump);
  newNameDefun(CorePkg, af_bignumP, core:bignump);
  newNameDefun(CorePkg, af_strP, core:strP);
  newNameDefun(CorePkg, af_doubleFloatP, core:doubleFloatP);
// clang-format off
  CoreDefun(baseCharP);
  SYMBOL_EXPORT_SC_(ClosPkg, classp);
  af_def(ClosPkg, "classp", &af_classp, ARGS_af_classp, DECL_af_classp, DOCS_af_classp);
  af_def(CorePkg, "builtInClassP", &core_builtInClassP, ARGS_core_builtInClassP, DECL_core_builtInClassP, DOCS_core_builtInClassP);
  CoreDefun(cxxInstanceP);
  Defun(lambda_list_handler_p);
  Defun(ratioP);
  Defun(longFloatP);
  Defun(shortFloatP);
  Defun(singleFloatP);
  Defun(pathP);
  Defun(hashTableP);
  ClDefun(readtablep);
  ClDefun(random_state_p);
  Defun(structureObjectP);
  Defun(singleDispatchGenericFunctionP);
  Defun(activation_frame_p);
  Defun(externalObjectP);
  //	Defun(sourceCodeConsP);
  Defun(interpretedFunctionP);
  Defun(properListP);
  Defun(pathnamep);
  Defun(logicalPathnameP);
};
};
