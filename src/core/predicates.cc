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

CL_DEFUN bool core__bignump(T_sp obj) {
  return gc::IsA<Bignum_sp>(obj);
};


CL_DEFUN bool cl__stringp(T_sp obj) {
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

CL_DEFUN bool clos__classp(T_sp obj) {
  return gc::IsA<Class_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("classp");
CL_DEFUN bool core__built_in_class_p(T_sp obj) {
  if (!clos__classp(obj))
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

CL_DEFUN bool cl__complexp(T_sp obj) {
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

CL_DEFUN bool cl__rationalp(T_sp obj) {
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

CL_DEFUN bool cl__realp(T_sp obj) {
  return gc::IsA<Real_sp>(obj);
};

CL_DEFUN bool cl__floatp(T_sp obj) {
  return gc::IsA<Float_sp>(obj);
};

CL_DEFUN bool cl__vectorp(T_sp obj) {
  return gc::IsA<Vector_sp>(obj);
};

CL_DEFUN bool cl__integerp(T_sp obj) {
  return gc::IsA<Integer_sp>(obj);
};

CL_DEFUN bool cl__keywordp(T_sp obj) {
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

CL_DEFUN bool core__arrayp(T_sp obj) {
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
    if (!cl__listp(fast)) {
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

  SYMBOL_EXPORT_SC_(ClosPkg, classp);
};
