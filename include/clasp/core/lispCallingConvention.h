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

#ifdef LCC_MACROS



namespace gctools {
  struct return_type {
#if (LCC_RETURN_VALUES_IN_REGISTERS!=1)
#error "The number of return values in registers does not match core::return_type"
#endif
    core::T_O* ret0[LCC_RETURN_VALUES_IN_REGISTERS];  // One for every LCC_RETURN_VALUES_IN_REGISTERS
    size_t nvals;
  return_type() : ret0{NULL}, nvals(0){};
  return_type(core::T_O *r0, size_t nv) : ret0{r0}, nvals(nv) {};
    template <typename T>
    return_type(T* r0, size_t nv) : ret0{reinterpret_cast<core::T_O*>(r0)}, nvals(nv) {};
  };
};

#define FILL_FRAME_WITH_RETURN_REGISTERS(frame,retval) \
  for ( size_t _i = 0, _iEnd(MIN(retval.nvals,LCC_RETURN_VALUES_IN_REGISTERS)); _i<_iEnd; ++_i) { \
    (*frame)[_i] = retval.ret0[_i]; \
  }
  
#define LCC_UNUSED NULL
#define LCC_FIXED_ARGS LCC_ARGS_IN_REGISTERS
#define LCC_FROM_SMART_PTR(x) (x.raw_())
#define LCC_TYPE T_O *
#define LCC_FROM_ACTIVATION_FRAME_SMART_PTR(x) (x.raw_())
#define LCC_TO_SMART_PTR(x) (gctools::smart_ptr<core::T_O>((gc::Tagged)x))

// Pass a defined number of arguments to operator()
#define LCC_PASS_ARGS0_ELLIPSIS(funcRaw)                funcRaw, 0, NULL, NULL, NULL, NULL
#define LCC_PASS_ARGS1_ELLIPSIS(funcRaw,a0)             funcRaw, 1, a0,   NULL, NULL, NULL
#define LCC_PASS_ARGS2_ELLIPSIS(funcRaw,a0, a1)         funcRaw, 2, a0,   a1,   NULL, NULL
#define LCC_PASS_ARGS3_ELLIPSIS(funcRaw,a0, a1, a2)     funcRaw, 3, a0,   a1,   a2,   NULL
#define LCC_PASS_ARGS4_ELLIPSIS(funcRaw,a0, a1, a2, a3) funcRaw, 4, a0,   a1,   a2,   a3
#define LCC_PASS_ARGS_ELLIPSIS_GENERAL(funcRaw,a0, a1, a2, a3) funcRaw, 4, a0,   a1,   a2,   a3

#define LCC_PASS_MAIN() NULL, 0, NULL, NULL, NULL, NULL

// Don't need lcc_arglist because no arguments are passed
#define LCC_PASS_ARGS0_VA_LIST(funcRaw) funcRaw, 0, NULL, NULL, NULL, NULL
#define LCC_PASS_ARGS1_VA_LIST(funcRaw,a0) funcRaw, 1, a0, NULL, NULL, NULL

#define LCC_UNUSED_rest0 NULL, NULL, NULL, NULL
#if 0
#define LCC_PASS_ARGS0_ARGLIST(funcRaw) funcRaw, 0, NULL, NULL, NULL, NULL
#define LCC_PASS_ARGS1_ARGLIST(funcRaw,a0) funcRaw, 1, a0, NULL, NULL, NULL
#define LCC_PASS_ARGS2_ARGLIST(funcRaw,a0, a1) funcRaw, 2, a0, a1, NULL, NULL
#define LCC_PASS_ARGS3_ARGLIST(funcRaw,a0, a1, a2) funcRaw, 3, a0, a1, a2, NULL
#define LCC_PASS_ARGS4_ARGLIST(funcRaw,a0, a1, a2, a3) funcRaw, 4, a0, a1, a2, a3
#define LCC_PASS_ARGS_ARGLIST_GENERAL(funcRaw, nargs, a0, a1, a2, a3) funcRaw, nargs, a0, a1, a2, a3
#endif

// To invoke "invoke" methods use these

#define LCC_ARGS_FUNCALL_ELLIPSIS core::T_O *lcc_closure, std::size_t lcc_nargs, core::T_O *lcc_fixed_arg0, core::T_O *lcc_fixed_arg1, core::T_O *lcc_fixed_arg2, core::T_O *lcc_fixed_arg3, ...
#define LCC_ARGS_CC_CALL_ELLIPSIS core::T_O *lcc_closure, std::size_t lcc_nargs, core::T_O *lcc_fixed_arg0, core::T_O *lcc_fixed_arg1, core::T_O *lcc_fixed_arg2, core::T_O *lcc_fixed_arg3, ...
#define LCC_ARGS_ELLIPSIS core::T_O *lcc_closure, std::size_t lcc_nargs, core::T_O *lcc_fixed_arg0, core::T_O *lcc_fixed_arg1, core::T_O *lcc_fixed_arg2,  core::T_O *lcc_fixed_arg3, ...
#define LCC_ARGS_LLH VaList_sp lcc_vargs, std::size_t lcc_nargs, core::T_O *lcc_fixed_arg0, core::T_O *lcc_fixed_arg1, core::T_O *lcc_fixed_arg2, core::T_O* lcc_fixed_arg3
// When you pass args to another function use LCC_PASS_ARGS
#define LCC_PASS_ARGS lcc_closure, lcc_nargs, lcc_fixed_arg0, lcc_fixed_arg1, lcc_fixed_arg2, lcc_fixed_arg3
#define LCC_PASS_ARGS_LLH lcc_vargs, lcc_nargs, lcc_fixed_arg0, lcc_fixed_arg1, lcc_fixed_arg2, lcc_fixed_arg3
#define LCC_PASS_ARGS_VASLIST(_closure,_vaslist) _closure, lcc_nargs, lcc_fixed_arg0, lcc_fixed_arg1, lcc_fixed_arg2, lcc_fixed_arg3, _vaslist

/*! This is a void function */
#define LISP_CALLING_CONVENTION() entry_point(LCC_ARGS_ELLIPSIS)
#define LISP_METHOD_CALLING_CONVENTION method_entry_point(LCC_ARGS_ELLIPSIS)
// Compiled functions get the raw va_list
#define LCC_VA_LIST(_valist) (*_valist)._Args
#define LCC_VA_START_ARG lcc_fixed_arg3
#define LCC_LAST_FIXED_ARG lcc_fixed_arg3


#define LCC_CLOSED_ENVIRONMENT core::T_O *lcc_closedEnvironment

#define LCC_VIRTUAL virtual
#define LCC_RETURN gc::return_type

// Return raw values that can be used to construct a core::T_mv
#define LCC_RETURN_RAW gctools::return_type


// To invoke functions of type InitFnPtr use these
#define LCC_PASS_ARGS0_VA_LIST_INITFNPTR() _Nil<core::T_O>().raw_(), 0, LCC_UNUSED_rest0

#define MULTIPLE_VALUES_ARRAY core::lisp_multipleValues()

/* ASSERT that the first argument is a VaList_sp */
#define ASSERT_FIRST_ARG_IS_VALIST() ASSERT(gctools::tagged_vaslistp(lcc_fixed_arg0))
#define LCC_ARG0_VALIST() gctools::smart_ptr<core::Vaslist>((gc::Tagged)lcc_fixed_arg0)
#define LCC_ARG0() gctools::smart_ptr<core::T_O>((gc::Tagged)lcc_fixed_arg0)
#define LCC_ARG1() gctools::smart_ptr<core::T_O>((gc::Tagged)lcc_fixed_arg1)
#define LCC_ARG2() gctools::smart_ptr<core::T_O>((gc::Tagged)lcc_fixed_arg2)
#define LCC_ARG3() gctools::smart_ptr<core::T_O>((gc::Tagged)lcc_fixed_arg3)
/*! LCC_ARGS_IN_REGISTERS is defined in src/core/config.h and is currently 4 (four)*/
#define LCC_FIXED_NUM LCC_ARGS_IN_REGISTERS
//#define MULTIPLE_VALUES_SETUP() core::T_sp* __multipleValuesPtr = core::lisp_multipleValues().start_address()

  
/* This is a switch statement that copies passed arguments in registers into the MultipleValues array */
#define LCC_SWITCH_TO_COPY_PASSED_ARGS_INTO_MULTIPLE_VALUES_ARRAY(_mv) \
  /* Fix me */ HARD_IMPLEMENT_ME(); \
  MultipleValues &_mv = lisp_callArgs();                               \
  _mv.setSize(lcc_nargs);                                              \
  switch (lcc_nargs) {                                                 \
  default:                                                             \
    for (int _zii = LCC_FIXED_ARGS; _zii < lcc_nargs; ++_zii) {        \
      _mv[_zii] = va_arg(lcc_arglist, LCC_TYPE);                       \
    }                                                                  \
    va_end(lcc_arglist);                                               \
  case 4:                                                              \
    _mv[3] = lcc_fixed_arg3;                                           \
  case 3:                                                              \
    _mv[2] = lcc_fixed_arg2;                                           \
  case 2:                                                              \
    _mv[1] = lcc_fixed_arg1;                                           \
  case 1:                                                              \
    _mv[0] = lcc_fixed_arg0;                                           \
  case 0:                                                              \
    break;                                                             \
  }

/*! This is X86_64 dependent code */
#if defined(X86_64) && defined(_ADDRESS_MODEL_64)

// This is VERY HACKISH
// it's based on the System V Application Binary Interface for X86_64
// I'm writing the register arguments into the reg_save_area and then
// resetting the gp_offset to point to the first register argument lcc_fixed_arg0
#define LCC_ABI_ARGS_IN_REGISTERS 6

#define ASSERT_LCC_VA_LIST_AT_START(_valist_s_) \
  ASSERT((_valist_s_)._Args->gp_offset == sizeof(uintptr_clasp_t) * (LCC_ABI_ARGS_IN_REGISTERS - LCC_ARGS_IN_REGISTERS));

// Registers are %rdi, %rsi, %rdx, %rcx, %r8, %r9
#define LCC_CLOSURE_REGISTER 0
#define LCC_NARGS_REGISTER 1
#define LCC_ARG0_REGISTER 2
#define LCC_ARGS_PASSED_IN_REGISTERS LCC_ARGS_IN_REGISTERS
#define LCC_ARG1_REGISTER 3
#define LCC_ARG2_REGISTER 4
#define LCC_ARG3_REGISTER 5
#define LCC_TOTAL_REGISTERS LCC_ABI_ARGS_IN_REGISTERS
#define LCC_SPILL_NUMBER_ARGUMENTS_TO_VA_LIST(_valist_s_, _num_)                               \
  {                                                                                            \
    ((uintptr_clasp_t *)(_valist_s_)._Args->reg_save_area)[LCC_NARGS_REGISTER] = (uintptr_clasp_t)(_num_); \
  }
#define LCC_SPILL_CLOSURE_TO_VA_LIST(_valist_s_,_closure_)    ((core::T_O* *)(_valist_s_)._Args->reg_save_area)[LCC_CLOSURE_REGISTER] = (core::T_O*)_closure_;

#define LCC_REGISTER_SAVE_AREA_CLOSURE(rsa) ((core::T_O**)rsa[LCC_CLOSURE_REGISTER])

#define LCC_SPILL_REGISTER_ARGUMENTS_TO_VA_LIST(_valist_s_) {                                                                          \
    ((core::T_O* *)(_valist_s_)._Args->reg_save_area)[LCC_CLOSURE_REGISTER] = (core::T_O*)lcc_closure;                                   \
    /* Tricky part!!! write the overflow_arg_area pointer into the reg_save_area */                                                    \
    /* so we can recover the overflow args even after the va_list has been traversed */                                                \
    /*((core::T_O* *)(_valist_s_)._Args->reg_save_area)[LCC_OVERFLOW_SAVE_REGISTER] = (core::T_O*)((_valist_s_)._Args->overflow_arg_area);*/ \
    ((core::T_O* *)(_valist_s_)._Args->reg_save_area)[LCC_NARGS_REGISTER] = (core::T_O*)lcc_nargs;                                       \
    ((core::T_O* *)(_valist_s_)._Args->reg_save_area)[LCC_ARG0_REGISTER] = (core::T_O*)lcc_fixed_arg0;                                   \
    ((core::T_O* *)(_valist_s_)._Args->reg_save_area)[LCC_ARG1_REGISTER] = (core::T_O*)lcc_fixed_arg1;                                   \
    ((core::T_O* *)(_valist_s_)._Args->reg_save_area)[LCC_ARG2_REGISTER] = (core::T_O*)lcc_fixed_arg2;                                   \
    ((core::T_O* *)(_valist_s_)._Args->reg_save_area)[LCC_ARG3_REGISTER] = (core::T_O*)lcc_fixed_arg3;                                   \
    (_valist_s_)._Args->gp_offset = sizeof(core::T_O*) * (LCC_ABI_ARGS_IN_REGISTERS - LCC_ARGS_IN_REGISTERS);                           \
  }

#define INITIALIZE_VA_LIST() ::core::Vaslist lcc_arglist_s(lcc_nargs);\
  va_start(lcc_arglist_s._Args, LCC_VA_START_ARG); \
  core::VaList_sp lcc_vargs(&lcc_arglist_s); \
  LCC_SPILL_REGISTER_ARGUMENTS_TO_VA_LIST(lcc_arglist_s); \

#define COPY_VA_LIST() \
  va_list passed_args; \
  va_start(passed_args,LCC_VA_START_ARG); \
  core::T_O* lcc_passed_valist = va_arg(passed_args,core::T_O*); \
  va_end(passed_args); \
  ::core::Vaslist lcc_arglist_s(*(core::Vaslist*)gctools::untag_vaslist(lcc_passed_valist));\
  core::VaList_sp lcc_vargs(&lcc_arglist_s); 

#define private_LCC_VA_LIST_TOTAL_NUMBER_OF_ARGUMENTS(_args) (size_t)(((uintptr_clasp_t *)(_args[0].reg_save_area))[LCC_NARGS_REGISTER])
#define private_LCC_VA_LIST_SET_TOTAL_NUMBER_OF_ARGUMENTS(_args, _n) (((uintptr_clasp_t *)(_args[0].reg_save_area))[LCC_NARGS_REGISTER]) = ((uintptr_clasp_t)_n)
//#define private_LCC_VA_LIST_DECREMENT_TOTAL_NUMBER_OF_ARGUMENTS(_args) (--((uintptr_clasp_t *)(_args[0].reg_save_area))[LCC_NARGS_REGISTER])

#define LCC_VA_LIST_CLOSURE(_args) core::Function_sp((gctools::Tagged)((core::T_O **)(*_args)._Args->reg_save_area)[LCC_CLOSURE_REGISTER])
#define LCC_VA_LIST_REGISTER_SAVE_AREA(_args) (core::T_O **)(((*_args))[0].reg_save_area)
#define LCC_VA_LIST_OVERFLOW_ARG_AREA(_args) (core::T_O **)(((*_args))[0].overflow_arg_area)
//#define LCC_ORIGINAL_VA_LIST_OVERFLOW_ARG_AREA(_args) ((core::T_O***)LCC_VA_LIST_REGISTER_SAVE_AREA(_args))[LCC_OVERFLOW_SAVE_REGISTER]
#define LCC_VA_LIST_TOTAL_NUMBER_OF_ARGUMENTS(_args) private_LCC_VA_LIST_TOTAL_NUMBER_OF_ARGUMENTS((*_args)._Args)
#define LCC_VA_LIST_SET_TOTAL_NUMBER_OF_ARGUMENTS(_args, _n) private_LCC_VA_LIST_SET_TOTAL_NUMBER_OF_ARGUMENTS((*_args)._Args, _n)
//#define LCC_VA_LIST_DECREMENT_TOTAL_NUMBER_OF_ARGUMENTS(_args) private_LCC_VA_LIST_DECREMENT_TOTAL_NUMBER_OF_ARGUMENTS((*_args)._Args)
#define LCC_VA_LIST_REGISTER_ARG0(_args) (((core::T_O **)(((*_args)._Args)[0].reg_save_area))[LCC_ARG0_REGISTER])
#define LCC_VA_LIST_REGISTER_ARG1(_args) (((core::T_O **)(((*_args)._Args)[0].reg_save_area))[LCC_ARG1_REGISTER])
#define LCC_VA_LIST_REGISTER_ARG2(_args) (((core::T_O **)(((*_args)._Args)[0].reg_save_area))[LCC_ARG2_REGISTER])
#define LCC_VA_LIST_REGISTER_ARG3(_args) (((core::T_O **)(((*_args)._Args)[0].reg_save_area))[LCC_ARG3_REGISTER])

#if 0
#define LCC_VA_LIST_CURRENT_INDEX(_res, _args)                    \
  _res = (((*_args)._Args[0].gp_offset/sizeof(void*))-LCC_ARG0_REGISTER); \
  if ( _res >= LCC_ARGS_PASSED_IN_REGISTERS ) { \
    _res = (LCC_VA_LIST_OVERFLOW_ARG_AREA(_args)-LCC_ORIGINAL_VA_LIST_OVERFLOW_ARG_AREA(_args)) + LCC_ARG0_REGISTER; \
  }

#define LCC_VA_LIST_REMAINING_NUMBER_OF_ARGUMENTS(_res, _args) \
  LCC_VA_LIST_CURRENT_INDEX(_res,_args); \
  _res = LCC_VA_LIST_TOTAL_NUMBER_OF_ARGUMENTS(_args) - (_res)

#endif


#if 0
#define LCC_VA_LIST_incorrect_INDEXED_ARG(_res, _args, _idx)                    \
  {                                                                   \
    int __x = (_idx) - ((48 - ((*_args)._Args[0].gp_offset)) / 8);     \
    if (__x < 0) {                                                    \
      _res = ((core::T_O **)(*_args)._Args[0].reg_save_area)[__x + 6]; \
    } else {                                                          \
      _res = ((core::T_O **)(*_args)._Args[0].overflow_arg_area)[__x]; \
    }                                                                 \
  }
#endif


/* Return the indexed argument in the va_list wrt the remaining arguments in the list.
   This means that if va_arg has already been called on this va_list  then the 0th
   entry is the next argument that would be returned by va_list */
#define LCC_VA_LIST_RELATIVE_INDEXED_ARG(_res, _args, _idx) { \
    int __x = (_idx) - (LCC_ABI_ARGS_IN_REGISTERS - (((*_args)._Args[0].gp_offset)/8)); \
    if (__x < 0) {                                                    \
      _res = ((core::T_O **)(*_args)._Args[0].reg_save_area)[__x + LCC_ABI_ARGS_IN_REGISTERS]; \
    } else {                                                          \
      _res = ((core::T_O **)(*_args)._Args[0].overflow_arg_area)[__x]; \
      /*      _res = ((core::T_O ***)(*_args)._Args[0].reg_save_area)[LCC_OVERFLOW_SAVE_REGISTER][__x]; */ \
    }                                                                 \
  }





//    _res = ((core::T_O **)(*_args)._Args[0].overflow_arg_area)[__x]; \

#define LCC_NEXT_ARG_RAW_AND_ADVANCE(arglist) va_arg((*arglist)._Args, core::T_O *)
//#define LCC_NEXT_ARG_AND_ADVANCE(arglist) core::T_sp((gc::Tagged)LCC_NEXT_ARG_RAW(arglist))
#endif // #if defined(X86) && defined(_ADDRESS_MODEL_64)



#define LCC_CALL_WITH_ARGS_IN_FRAME(_result, _closure, _frame)          \
  core::Vaslist valist_s(_frame);                                      \
  printf("%s:%d  This needs to be reimplemented to handle the new calling convention\n", __FILE__, __LINE__ ); \
  LCC_SPILL_CLOSURE_TO_VA_LIST(valist_s,_closure.raw_());                \
  size_t lcc_nargs = (_frame).number_of_arguments();                    \
  core::T_O *lcc_arglist = valist_s.asTaggedPtr();                      \
  switch (lcc_nargs) {                                                  \
  default:                                                              \
      _result = _closure->invoke_va_list(LCC_PASS_ARGS_ELLIPSIS_GENERAL(_closure.raw_(), lcc_nargs, (_frame)[0], (_frame)[1], (_frame)[2], (_frame)[3])); \
    break;                                                              \
  case 4:                                                               \
      _result = _closure->invoke_va_list(LCC_PASS_ARGS4_ELLIPSIS(_closure.raw_(),(_frame)[0], (_frame)[1], (_frame)[2], (_frame)[3])); \
    break;                                                              \
  case 3:                                                               \
      _result = _closure->invoke_va_list(LCC_PASS_ARGS3_ELLIPSIS(_closure.raw_(),(_frame)[0], (_frame)[1], (_frame)[2])); \
    break;                                                              \
  case 2:                                                               \
      _result = _closure->invoke_va_list(LCC_PASS_ARGS2_ELLIPSIS(_closure.raw_(),(_frame)[0], (_frame)[1])); \
    break;                                                              \
  case 1:                                                               \
      _result = _closure->invoke_va_list(LCC_PASS_ARGS1_ELLIPSIS(_closure.raw_(),(_frame)[0])); \
    break;                                                              \
  case 0:                                                               \
      _result = _closure->invoke_va_list(LCC_PASS_ARGS0_ELLIPSIS(_closure.raw_())); \
    break;                                                              \
  };


#define LCC_DECLARE_VA_LIST()                           \
  Vaslist lcc_arglist_struct(lcc_nargs);               \
  va_start(lcc_arglist_struct._Args, LCC_VA_START_ARG); \
  VaList_sp lcc_arglist(&lcc_arglist_struct);

/*! Initialize a Vaslist struct from a Frame object */
#define LCC_SETUP_VA_LIST_FROM_FRAME(_va_list_, _frame_) { \
    (_va_list_)[0].reg_save_area = (_frame_).reg_save_area_ptr(); \
    (_va_list_)[0].overflow_arg_area = (_frame_).overflow_arg_area_ptr(); \
    (_va_list_)[0].gp_offset = (LCC_ABI_ARGS_IN_REGISTERS-LCC_ARGS_IN_REGISTERS) * sizeof(gc::Frame::ElementType); \
    (_va_list_)[0].fp_offset = 304; \
  }

/*! Rewind the general pointer area for a va_list to the first required argument */
#define LCC_REWIND_VA_LIST_KEEP_REGISTER_SAVE_AREA(_va_list_) { \
    (_va_list_)[0].gp_offset = 16; \
  }

/*! Rewind the general pointer area for a va_list to the first required argument */
#define LCC_REWIND_VA_LIST(_va_list_, _register_save_areaP_) { \
    (_va_list_)[0].reg_save_area = (void*)_register_save_areaP_; \
    (_va_list_)[0].gp_offset = 16; \
  }

#define LCC_SETUP_VA_LIST_FROM_VA_LIST(_dest_,_src_) va_copy(_dest_,_src_)

// Create a VaList_sp from lcc_vargs (a va_list)
//#define LCC_MAKE_VA_LIST_SP(valist_sp) VaList_sp valist_sp(lcc_nargs,lcc_vargs);
#endif // #ifdef LCC_MACROS

#define SETUP_CLOSURE_(Type,var) Type* var = gctools::untag_general<Type*>((Type*)lcc_closure);
#ifdef _DEBUG_BUILD
#define SETUP_CLOSURE(Type,var) \
  if (!gc::TaggedCast<Type*,core::T_O*>::isA(lcc_closure)) { \
    SIMPLE_ERROR_SPRINTF("Bad cast of closure %p to type: %s", (void*)lcc_closure, #Type ); \
  } \
  SETUP_CLOSURE_(Type,var);
#else
#define SETUP_CLOSURE(Type,var) SETUP_CLOSURE_(Type,var)
#endif                
#ifdef LCC_PROTOTYPES
typedef LCC_RETURN_RAW (*claspFunction)(LCC_ARGS_ELLIPSIS);
typedef LCC_RETURN_RAW (*fnLispCallingConvention)(LCC_ARGS_ELLIPSIS);
typedef LCC_RETURN_RAW (*CompiledClosure_fptr_type)(LCC_ARGS_ELLIPSIS);
typedef LCC_RETURN (*InitFnPtr)(LCC_ARGS_ELLIPSIS);
typedef LCC_RETURN (*DispatchFunction_fptr_type)(gctools::Tagged gf, gctools::Tagged valist_sptr);
typedef LCC_RETURN (*ShutdownFunction_fptr_type)();
#endif


#ifdef LCC_FUNCALL
extern "C" {

// Return true if the Vaslist is at the head of the list and false if it is used up
inline void dump_va_list(va_list val) {
  const char* atpos = "";
  if (val[0].gp_offset==0x10) atpos = "atStartReg";
  else if (val[0].gp_offset==0x30) atpos = "pastEnd";
  printf("           gp_offset = %p (%s)\n", reinterpret_cast<void*>(val[0].gp_offset),  atpos );
  printf("           fp_offset = %p\n", reinterpret_cast<void*>(val[0].fp_offset) );
  printf("   overflow_arg_area = %p\n", reinterpret_cast<void*>(val[0].overflow_arg_area) );
  printf("       reg_save_area = %p\n", reinterpret_cast<void*>(val[0].reg_save_area) );
  printf("---Register save area@%p (NOTE: Often in other stack frame)\n", val[0].reg_save_area);
  printf("       CLOSURE_REGISTER@%p = %p\n", reinterpret_cast<void*>(LCC_CLOSURE_REGISTER*8), ((core::T_O* *)val[0].reg_save_area)[LCC_CLOSURE_REGISTER] );
  printf("         NARGS_REGISTER@%p = %zu\n", reinterpret_cast<void*>(LCC_NARGS_REGISTER*8),reinterpret_cast<size_t>(((core::T_O* *)val[0].reg_save_area)[LCC_NARGS_REGISTER]) );
  printf("          ARG0_REGISTER@%p = %p\n", reinterpret_cast<void*>(LCC_ARG0_REGISTER*8), ((core::T_O* *)val[0].reg_save_area)[LCC_ARG0_REGISTER] );
  printf("          ARG1_REGISTER@%p = %p\n", reinterpret_cast<void*>(LCC_ARG1_REGISTER*8), ((core::T_O* *)val[0].reg_save_area)[LCC_ARG1_REGISTER] );
  printf("          ARG2_REGISTER@%p = %p\n", reinterpret_cast<void*>(LCC_ARG2_REGISTER*8), ((core::T_O* *)val[0].reg_save_area)[LCC_ARG2_REGISTER] );
  printf("          ARG3_REGISTER@%p = %p\n", reinterpret_cast<void*>(LCC_ARG3_REGISTER*8), ((core::T_O* *)val[0].reg_save_area)[LCC_ARG3_REGISTER] );
};


// Return true if the Vaslist is at the head of the list and false if it is used up
inline bool dump_Vaslist_ptr(Vaslist* args_orig) {
  Vaslist args(*args_orig);
  printf("va_list dump @%p\n", (void*)args_orig);
  printf("va_list remaining_args = %lu\n", args.remaining_nargs());
  bool atHead = (args.remaining_nargs() != args.total_nargs());
  dump_va_list((args)._Args);
  int iEnd = args.remaining_nargs();
  if (iEnd>CALL_ARGUMENTS_LIMIT) {
    iEnd = 0;
    printf("%s:%d      args.remaining_nargs()-> %lu !!!!  A BAD VALUE\n", __FILE__, __LINE__, args.remaining_nargs());
  }
  for ( int i=0; i<iEnd; ++i ) {
    T_O* arg = args.next_arg_raw();
    printf("     [%d] --> %p\n", i, arg);
  }
  return atHead;
};
};



#define APPLY_TO_VA_LIST
#include <clasp/core/applyToFrame.h>
#undef APPLY_TO_VA_LIST
    
                       

/*! Call a function object with args in a VaList_sp and consume the valist.
The Callee can NOT use args after this call.
Note: Since we don't have the full Function_O class definition when this
header is compiled (and we don't want to #include it because of all the problems
that will cause) I made this a template function where you pass the function
type as a template argument.  Call it like this...
funcall_consume_valist_<core::Function_O>(tagged_func_ptr,valist_args)
*/
template <typename Func_O_Type>
inline gctools::return_type funcall_consume_valist_(gc::Tagged func_tagged, VaList_sp args) {
  ASSERT(gc::tagged_generalp(func_tagged));
  gc::smart_ptr<Function_O> func((gc::Tagged)func_tagged);
  size_t nargs = args->remaining_nargs();
  switch (nargs) {
#define APPLY_TO_VA_LIST_CASE 1
#include <clasp/core/applyToFrame.h>
#undef APPLY_TO_VA_LIST_CASE
  default:
      printf("%s:%d Called %s with %lu arguments - add support for calls with this arity or reduce the number of args in this call\n", __FILE__, __LINE__, _rep_(func).c_str(), nargs);
      SIMPLE_ERROR_SPRINTF("Illegal arity %lu for funcall",  nargs);
      break;
  }
  SIMPLE_ERROR_SPRINTF("Unsupported arity %lu must be less than %lu",  nargs, CALL_ARGUMENTS_LIMIT );
}
/*! Return (values arguments closure) */
T_mv capture_arguments(uintptr_t functionAddress, uintptr_t basePointer, int frameOffset);



#endif // LCC_FUNCALL
