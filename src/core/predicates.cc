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

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("baseCharP");
CL_DEFUN bool core__base_char_p(T_sp arg) {
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

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("Return true if argument is a simple-string");
CL_DEFUN bool core__simple_string_p(T_sp obj) {
  return gc::IsA<Str_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("doubleFloatP");
CL_DEFUN bool core__double_float_p(T_sp obj) {
  return gc::IsA<DoubleFloat_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("functionP");
CL_DEFUN bool cl__functionp(T_sp obj) {
  if (Instance_sp inst_obj = obj.asOrNull<Instance_O>()) {
    return inst_obj->isgf();
  }
  return gc::IsA<Function_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("interpretedFunctionP");
CL_DEFUN bool core__interpreted_function_p(T_sp arg) {
  if (gc::IsA<Function_sp>(arg)) {
    if (auto intfunc = gc::As<Function_sp>(arg)->closure.asOrNull<InterpretedClosure>()) {
      (void)intfunc;
      return true;
    }
  }
  return false;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("See CLHS packagep");
CL_DEFUN bool cl__packagep(T_sp obj) {
  return gc::IsA<Package_sp>(obj);
};

#define ARGS_af_classp "(arg)"
#define DECL_af_classp ""
#define DOCS_af_classp "classp"
bool af_classp(T_sp obj) {
  return gc::IsA<Class_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("classp");
CL_DEFUN bool core__built_in_class_p(T_sp obj) {
  if (!af_classp(obj))
    return false;
  Class_sp c = gc::As<Class_sp>(obj);
  return c->builtInClassP();
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("classp");
CL_DEFUN bool core__cxx_instance_p(T_sp obj) {
  if (Instance_sp ip = obj.asOrNull<Instance_O>()) {
    (void)ip;
    return true;
  }
  return false;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("lambda_list_handler_p");
CL_DEFUN bool core__lambda_list_handler_p(T_sp obj) {
  return gc::IsA<LambdaListHandler_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("numberP");
CL_DEFUN bool cl__numberp(T_sp obj) {
  return gc::IsA<Number_sp>(obj);
};

#define ARGS_af_complexP "(arg)"
#define DECL_af_complexP ""
#define DOCS_af_complexP "complexP"
bool af_complexP(T_sp obj) {
  return gc::IsA<Complex_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("ratioP");
CL_DEFUN bool core__ratio_p(T_sp obj) {
  return gc::IsA<Ratio_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("ratioP");
CL_DEFUN bool cl__random_state_p(T_sp obj) {
  return gc::IsA<RandomState_sp>(obj);
};

#define ARGS_af_rationalP "(arg)"
#define DECL_af_rationalP ""
#define DOCS_af_rationalP "rationalP"
bool af_rationalP(T_sp obj) {
  return gc::IsA<Rational_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("longFloatP");
CL_DEFUN bool core__long_float_p(T_sp obj) {
  return gc::IsA<LongFloat_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("shortFloatP");
CL_DEFUN bool core__short_float_p(T_sp obj) {
  return gc::IsA<ShortFloat_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("singleFloatP");
CL_DEFUN bool core__single_float_p(T_sp obj) {
  return gc::IsA<SingleFloat_sp>(obj);
};

#define ARGS_af_realP "(arg)"
#define DECL_af_realP ""
#define DOCS_af_realP "realP"
bool af_realP(T_sp obj) {
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

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("pathP");
CL_DEFUN bool core__path_p(T_sp obj) {
  return gc::IsA<Path_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("bitVectorP");
CL_DEFUN bool cl__bit_vector_p(T_sp obj) {
  return gc::IsA<BitVector_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("hashTableP");
CL_DEFUN bool cl__hash_table_p(T_sp obj) {
  if (HashTable_sp ht = obj.asOrNull<HashTable_O>()) {
    (void)ht;
    return true;
  }
  return false;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("readtablep");
CL_DEFUN bool cl__readtablep(T_sp obj) {
  return gc::IsA<ReadTable_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("structureObjectP");
CL_DEFUN bool core__structure_object_p(T_sp obj) {
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

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("singleDispatchGenericFunctionP");
CL_DEFUN bool core__single_dispatch_generic_function_p(T_sp obj) {
  return gc::IsA<SingleDispatchGenericFunction_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("activation_frame_p");
CL_DEFUN bool core__activation_frame_p(T_sp obj) {
  return gc::IsA<ActivationFrame_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("externalObjectP");
CL_DEFUN bool core__external_object_p(T_sp obj) {
  return gc::IsA<ExternalObject_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("simple_bit_vector_p");
CL_DEFUN bool cl__simple_bit_vector_p(T_sp o) {
  if (SimpleBitVector_sp sbv = o.asOrNull<SimpleBitVector_O>()) {
    (void)sbv;
    return true;
  }
  return false;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("simple_vector_p");
CL_DEFUN bool cl__simple_vector_p(T_sp o) {
  if (VectorObjects_sp vo = o.asOrNull<VectorObjects_O>()) {
    (void)vo;
    return true;
  }
  return false;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("compiled_function_p");
CL_DEFUN bool cl__compiled_function_p(T_sp o) {
  if (Function_sp fn = o.asOrNull<Function_O>()) {
    (void)fn;
    return fn->closure->compiledP();
  }
  return false;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("genericFunctionP");
CL_DEFUN bool core__generic_function_p(T_sp o) {
  if (Function_sp cf = o.asOrNull<Function_O>()) {
    (void)cf;
    IMPLEMENT_MEF(BF("I should have a more sophisticated test here"));
    return true;
  }
  return false;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("Return true if arg is a proper list");
CL_DEFUN bool core__proper_list_p(T_sp arg) {
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

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("pathnamep");
CL_DEFUN bool cl__pathnamep(T_sp obj) {
  return gc::IsA<Pathname_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("logicalPathnameP");
CL_DEFUN bool core__logical_pathname_p(T_sp obj) {
  return gc::IsA<LogicalPathname_sp>(obj);
};

void initialize_predicates() {
  af_def(ClPkg, "endp", &cl__endp);
#define newNameDefun(pkg, myname, lispname) af_def(pkg, #lispname, &myname, ARGS_##myname, DECL_##myname, DOCS_##myname)
  af_def(ClPkg, "symbolp", &cl_symbolp);
  af_def(ClPkg, "consp", &cl_consp);
  af_def(ClPkg, "listp", &cl_listp);
// clang-format off
  newNameDefun(CorePkg, af_integerP, cl:integerp);
  newNameDefun(CorePkg, af_rationalP, cl:rationalp);
  newNameDefun(CorePkg, af_floatP, cl:floatp);
  newNameDefun(CorePkg, af_realP, cl:realp);
  newNameDefun(CorePkg, af_complexP, cl:complexp);
  newNameDefun(CorePkg, af_characterP, cl:characterp);
  newNameDefun(CorePkg, af_stringP, cl:stringp);
//  newNameDefun(CorePkg, cl__bit_vector_p, cl:bit_vector_p);
  newNameDefun(CorePkg, af_vectorP, cl:vectorp);
//  newNameDefun(CorePkg, cl__simple_vector_p, cl:simple_vector_p);
//  newNameDefun(CorePkg, core__str_p, cl:simple_string_p);
//  newNameDefun(CorePkg, cl__simple_bit_vector_p, cl:simple_bit_vector_p);
  newNameDefun(CorePkg, af_arrayP, cl:arrayp);
//  newNameDefun(CorePkg, cl__packagep, cl:packagep);
//  newNameDefun(CorePkg, cl__functionp, cl:functionp);
//  newNameDefun(CorePkg, cl__compiled_function_p, cl:compiled_function_p);
//  newNameDefun(CorePkg, core__generic_function_p, core:genericFunctionP);
  newNameDefun(CorePkg, af_keywordP, cl:keywordp);
//  newNameDefun(CorePkg, cl__atom, cl:atom);
  newNameDefun(CorePkg, af_fixnumP, core:fixnump);
  newNameDefun(CorePkg, af_bignumP, core:bignump);
//  newNameDefun(CorePkg, core__str_p, core:strP);
//  newNameDefun(CorePkg, core__double_float_p, core:doubleFloatP);
// clang-format off
  SYMBOL_EXPORT_SC_(ClosPkg, classp);
  af_def(ClosPkg, "classp", &af_classp, ARGS_af_classp, DECL_af_classp, DOCS_af_classp);
//  af_def(CorePkg, "builtInClassP", &core__built_in_class_p, ARGS_core__built_in_class_p, DECL_core__built_in_class_p, DOCS_core__built_in_class_p);
  //	Defun(sourceCodeConsP);
};
};
