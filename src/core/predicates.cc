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
//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lispList.h>
#include <clasp/core/metaClass.h>
#include <clasp/core/array.h>
#include <clasp/core/package.h>
#include <clasp/core/bignum.h>
#include <clasp/core/closPackage.h>
#include <clasp/core/pathname.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/random.h>
#include <clasp/core/structureObject.h>
#include <clasp/core/instance.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/readtable.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/numbers.h>
#include <clasp/core/externalObject.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/wrappers.h>

namespace core {

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("baseCharP");
CL_DEFUN bool core__base_char_p(T_sp arg) {
  return (arg.characterp()&&clasp_base_char_p(arg.unsafe_character()));
};

CL_DEFUN bool core__bignump(T_sp obj) {
  return gc::IsA<Bignum_sp>(obj);
};

// XXX: this should be adjusted whenever unicode is implemented
CL_DEFUN bool core__base_string_p(T_sp obj) {
  return gc::IsA<Str8Ns_sp>(obj) || gc::IsA<SimpleBaseString_sp>(obj);
};

CL_DEFUN bool cl__stringp(T_sp obj) {
  return gc::IsA<SimpleString_sp>(obj) || gc::IsA<StrNs_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("Return true if argument is a simple-string");
CL_DEFUN bool cl__simple_string_p(T_sp obj) {
  return gc::IsA<SimpleString_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("Return true if argument is a simple-string");
CL_DEFUN bool core__extended_string_p(T_sp obj) {
  return gc::IsA<SimpleCharacterString_sp>(obj)||gc::IsA<StrWNs_sp>(obj);
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
  return gc::IsA<Function_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("interpretedFunctionP");
CL_DEFUN bool core__interpreted_function_p(T_sp arg) {
  if ( auto intfunc = arg.asOrNull<InterpretedClosure_O>() ) {
    (void)intfunc;
    return true;
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
  if (gc::IsA<Instance_sp>(obj)) {
    Instance_sp iobj = gc::As_unsafe<Instance_sp>(obj);
    if (iobj->_Class == _lisp->_Roots._TheStandardClass
        || iobj->_Class == _lisp->_Roots._TheClass
        || iobj->_Class == _lisp->_Roots._TheBuiltInClass
        || iobj->_Class == _lisp->_Roots._TheStructureClass
        || iobj->_Class == _lisp->_Roots._TheClassRep
        || iobj->_Class == _lisp->_Roots._TheDerivableCxxClass ) return true;
    return core__subclassp(iobj->_Class,_lisp->_Roots._TheClass);
  }
  return false;
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
  return obj.fixnump()||obj.single_floatp()||gc::IsA<Number_sp>(obj);
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
  return obj.fixnump()||gc::IsA<Rational_sp>(obj);
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
  return obj.single_floatp();
};

CL_DEFUN bool cl__realp(T_sp obj) {
  return obj.fixnump()||obj.single_floatp()||gc::IsA<Real_sp>(obj);
};

CL_DEFUN bool cl__floatp(T_sp obj) {
  return obj.single_floatp()||gc::IsA<Float_sp>(obj);
};

CL_DEFUN bool cl__vectorp(T_sp obj) {
  return gc::IsA<AbstractSimpleVector_sp>(obj)
    || (gc::IsA<MDArray_sp>(obj)&&gc::As_unsafe<MDArray_sp>(obj)->rank()==1);
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
CL_DOCSTRING("bitVectorP");
CL_DEFUN bool cl__bit_vector_p(T_sp obj) {
  return gc::IsA<SimpleBitVector_sp>(obj)||gc::IsA<BitVector_sp>(obj);
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
#if 0
  if (gc::IsA<StructureObject_sp>(obj)) return true;
#endif
  return gc::IsA<Instance_sp>(obj);
};

CL_DEFUN bool core__arrayp(T_sp obj) {
  return gc::IsA<Array_sp>(obj);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("singleDispatchGenericFunctionP");
CL_DEFUN bool core__single_dispatch_generic_function_p(T_sp obj) {
  return gc::IsA<SingleDispatchGenericFunctionClosure_sp>(obj);
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
  return gc::IsA<SimpleBitVector_sp>(o);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("simple_vector_p");
CL_DEFUN bool cl__simple_vector_p(T_sp o) {
  return gc::IsA<SimpleVector_sp>(o);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("compiled_function_p");
CL_DEFUN bool cl__compiled_function_p(T_sp o) {
  if (Closure_sp fn = o.asOrNull<Closure_O>()) {
    (void)fn;
    return fn->compiledP();
  } else if (gc::IsA<FuncallableInstance_sp>(o)) {
    return true;
  }
  return false;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("genericFunctionP");
CL_DEFUN bool core__generic_function_p(T_sp o) {
  if (gc::IsA<FuncallableInstance_sp>(o)) {
    return gc::As_unsafe<FuncallableInstance_sp>(o)->isgf();
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


namespace ext {
CL_NAME("EXT:LOCAL-FUNCTION-FORM-P");
CL_DEFUN bool local_function_form_p(core::T_sp form)
{
  return ((form).consp() &&
          (core::oCar(gc::As<core::Cons_sp>(form)) == cl::_sym_flet ||
           core::oCar(gc::As<core::Cons_sp>(form)) == cl::_sym_labels ));
}

};
