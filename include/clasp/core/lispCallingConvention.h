/*
    File: lispCallingConvention.h
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

#define LCC_UNUSED NULL
#define LCC_FIXED_ARGS LCC_ARGS_IN_REGISTERS
#define LCC_FROM_SMART_PTR(x) (x.raw_())
#define LCC_TYPE T_O*
#define LCC_FROM_ACTIVATION_FRAME_SMART_PTR(x) (x.raw_())
#define LCC_TO_SMART_PTR(x) (gctools::smart_ptr<core::T_O>((gc::Tagged)x))

#define LCC_UNUSED_1() NULL
#define LCC_UNUSED_2() NULL, NULL
#define LCC_UNUSED_3() NULL, NULL, NULL
#define LCC_UNUSED_rest2() NULL
#define LCC_UNUSED_rest1() NULL, NULL
#define LCC_UNUSED_rest0() NULL, NULL, NULL

// Pass a defined number of arguments to operator()
#define LCC_PASS_ARGS0_ELLIPSIS() 0, LCC_UNUSED_rest0()
#define LCC_PASS_ARGS1_ELLIPSIS(a0) 1, a0, LCC_UNUSED_rest1()
#define LCC_PASS_ARGS2_ELLIPSIS(a0, a1) 2, a0, a1, LCC_UNUSED_rest2()
#define LCC_PASS_ARGS3_ELLIPSIS(a0, a1, a2) 3, a0, a1, a2

// To invoke "invoke" methods use these
#define LCC_PASS_ARGS0_VA_LIST() 0, LCC_UNUSED_rest0(), NULL
#define LCC_PASS_ARGS1_VA_LIST(a0) 1, a0, LCC_UNUSED_rest1(), NULL
#define LCC_PASS_ARGS2_VA_LIST(a0, a1) 2, a0, a1, LCC_UNUSED_rest2(), NULL
#define LCC_PASS_ARGS3_VA_LIST(a0, a1, a2) 3, a0, a1, a2, NULL


#define LCC_ARGS_ELLIPSIS std::size_t lcc_nargs, core::T_O *lcc_fixed_arg0, core::T_O *lcc_fixed_arg1, core::T_O *lcc_fixed_arg2, ...
#define LCC_ARGS_VA_LIST std::size_t lcc_nargs, core::T_O *lcc_fixed_arg0, core::T_O *lcc_fixed_arg1, core::T_O *lcc_fixed_arg2, va_list lcc_arglist
// When you pass args to another function use LCC_PASS_ARGS
#define LCC_PASS_ARGS lcc_nargs, lcc_fixed_arg0, lcc_fixed_arg1, lcc_fixed_arg2, lcc_arglist
#define LCC_LAST_FIXED_ARG lcc_fixed_arg2


#define LCC_CLOSED_ENVIRONMENT core::T_O *lcc_closedEnvironment

#define LCC_RETURN core::T_mv

// Return raw values that can be used to construct a core::T_mv
#define LCC_RETURN_RAW gc::return_type


/*! This is a void function */
#define LISP_CALLING_CONVENTION() invoke_va_list(LCC_ARGS_VA_LIST)

typedef LCC_RETURN_RAW (*fnLispCallingConvention)(LCC_CLOSED_ENVIRONMENT, LCC_ARGS_VA_LIST);
typedef LCC_RETURN_RAW (*CompiledClosure_fptr_type)(LCC_CLOSED_ENVIRONMENT,LCC_ARGS_VA_LIST);
typedef LCC_RETURN (*InitFnPtr)(LCC_CLOSED_ENVIRONMENT, LCC_ARGS_VA_LIST);

// To invoke functions of type InitFnPtr use these
#define LCC_PASS_ARGS0_VA_LIST_INITFNPTR() NULL, 0, LCC_UNUSED_rest0(), NULL
#define LCC_PASS_ARGS1_VA_LIST_INITFNPTR(a0) NULL, 1, a0, LCC_UNUSED_rest1(), NULL
#define LCC_PASS_ARGS2_VA_LIST_INITFNPTR(a0, a1) NULL, 2, a0, a1, LCC_UNUSED_rest2(), NULL
#define LCC_PASS_ARGS3_VA_LIST_INITFNPTR(a0, a1, a2) NULL, 3, a0, a1, a2, NULL



#define MULTIPLE_VALUES_ARRAY core::lisp_multipleValues()

#define LCC_ARG0() gctools::smart_ptr<core::T_O>((gc::Tagged)lcc_fixed_arg0)
#define LCC_ARG1() gctools::smart_ptr<core::T_O>((gc::Tagged)lcc_fixed_arg1)
#define LCC_ARG2() gctools::smart_ptr<core::T_O>((gc::Tagged)lcc_fixed_arg2)
/*! LCC_ARGS_IN_REGISTERS is defined in src/core/config.h and is currently 4 (four)*/
#define LCC_FIXED_NUM LCC_ARGS_IN_REGISTERS
//#define MULTIPLE_VALUES_SETUP() core::T_sp* __multipleValuesPtr = core::lisp_multipleValues().start_address()
#if 0  // Convert these to use va_list
#define LCC_ARG3() (MULTIPLE_VALUES_ARRAY[3])
#define LCC_ARG4() (MULTIPLE_VALUES_ARRAY[4])
#define LCC_ARG5() (MULTIPLE_VALUES_ARRAY[5])
#define LCC_ARG6() (MULTIPLE_VALUES_ARRAY[6])
#define LCC_ARG7() (MULTIPLE_VALUES_ARRAY[7])
#define LCC_ARG8() (MULTIPLE_VALUES_ARRAY[8])
#define LCC_ARG9() (MULTIPLE_VALUES_ARRAY[9])
#define LCC_ARG10() (MULTIPLE_VALUES_ARRAY[10])
#define LCC_ARG11() (MULTIPLE_VALUES_ARRAY[11])
#define LCC_ARG12() (MULTIPLE_VALUES_ARRAY[12])
#define LCC_ARG13() (MULTIPLE_VALUES_ARRAY[13])
#define LCC_ARG14() (MULTIPLE_VALUES_ARRAY[14])
#define LCC_ARG15() (MULTIPLE_VALUES_ARRAY[15])
#define LCC_ARG16() (MULTIPLE_VALUES_ARRAY[16])
#define LCC_ARG17() (MULTIPLE_VALUES_ARRAY[17])
#define LCC_ARG18() (MULTIPLE_VALUES_ARRAY[18])
#define LCC_ARG19() (MULTIPLE_VALUES_ARRAY[19])
#define LCC_ARG20() (MULTIPLE_VALUES_ARRAY[20])
#endif

/* This is a switch statement that copies passed arguments in registers into the MultipleValues array */
#define LCC_SWITCH_TO_COPY_PASSED_ARGS_INTO_MULTIPLE_VALUES_ARRAY(_mv) \
  MultipleValues &_mv = lisp_callArgs();                               \
  _mv.setSize(lcc_nargs);                                              \
  switch (lcc_nargs) {                                                 \
  default:                                                             \
      for ( int _zii = LCC_FIXED_ARGS; _zii < lcc_nargs; ++_zii) {     \
        _mv[_zii] = va_arg(lcc_arglist,LCC_TYPE);                     \
      }                                                                \
      va_end(lcc_arglist);                                             \
  case 3:                                                              \
      _mv[2] = lcc_fixed_arg2;                                         \
  case 2:                                                              \
      _mv[1] = lcc_fixed_arg1;                                         \
  case 1:                                                              \
      _mv[0] = lcc_fixed_arg0;                                         \
  case 0:                                                              \
    break;                                                             \
  }


#define LCC_DECLARE_VA_LIST() \
  va_list lcc_arglist; \
  va_start(lcc_arglist,LCC_LAST_FIXED_ARG); 

/*! This is X86_64 dependent code */
#if defined(X86) && defined(_ADDRESS_MODEL_64)

#define LCC_SPILL_REGISTER_ARGUMENTS_TO_VA_LIST() { \
    ((uintptr_t*)lcc_arglist->reg_save_area)[0] = (uintptr_t)lcc_nargs; \
    ((uintptr_t*)lcc_arglist->reg_save_area)[1] = (uintptr_t)lcc_fixed_arg0; \
    ((uintptr_t*)lcc_arglist->reg_save_area)[2] = (uintptr_t)lcc_fixed_arg1; \
    ((uintptr_t*)lcc_arglist->reg_save_area)[3] = (uintptr_t)lcc_fixed_arg2; \
  }


#define LCC_DECLARE_VA_LIST_AND_SPILL_REGISTER_ARGUMENTS_TO_VA_LIST() \
  LCC_DECLARE_VA_LIST(); \
  LCC_SPILL_REGISTER_ARGUMENTS_TO_VA_LIST(lcc_nargs, lcc_fixed_arg0, lcc_fixed_arg1, lcc_fixed_arg2, lcc_arglist);

#define LCC_VA_LIST_NUMBER_OF_ARGUMENTS(arglist) ((size_t)(arglist->reg_save_area))[0])

#define LCC_INDEXED_ARG(arglist,arg_idx) gc::smart_ptr<core::T_O>((gctools::Tagged)((arg_idx<LCC_FIXED_ARGS) ? (((uintptr_t*)(arglist->reg_save_area))[1+arg_idx]) : (((uintptr_t*)(arglist->overflow_arg_area))[arg_idx-LCC_FIXED_ARGS])))

#else
#error "Add support for accessing LCC_INDEXED_ARG"
#endif


#define LCC_VA_LIST_TO_VECTOR_OBJECTS(_valist,_vec) \
  core::VectorObjects_sp _vec = core::VectorObjects_O::create(_Nil<core::T_O>(),LCC_VA_LIST_NUMBER_OF_ARGUMENTS(_valist)); \
  switch (_vec->vector_length()) {                                       \
  default:                                                             \
      for ( int _zii = LCC_FIXED_ARGS, _zend(_vec->vector_length()); _zii < _zend; ++_zii) { \
              _vec->setf_elt(_zii,LCC_INDEXED_ARG(_valist,_zii)); \
      }                                                                \
  case 3:                                                              \
      _vec->setf_elt(2,LCC_INDEXED_ARG(_valist,2); \
  case 2:                                                              \
      _vec->setf_elt(1,LCC_INDEXED_ARG(_valist,1); \
  case 1:                                                              \
      _vec->setf_elt(0,LCC_INDEXED_ARG(_valist,0); \
  case 0:                                                              \
    break;                                                             \
  }
