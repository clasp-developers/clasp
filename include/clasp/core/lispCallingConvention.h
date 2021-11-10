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
#if (LCC_RETURN_VALUES_IN_REGISTERS()!=1)
#error "The number of return values in registers does not match core::return_type"
#endif
    void* ret0[LCC_RETURN_VALUES_IN_REGISTERS()];  // One for every LCC_RETURN_VALUES_IN_REGISTERS
    size_t nvals;
  return_type() : ret0{NULL}, nvals(0){};
  return_type(core::T_O *r0, size_t nv) : ret0{r0}, nvals(nv) {};
    template <typename T>
    return_type(T* r0, size_t nv) : ret0{reinterpret_cast<void*>(r0)}, nvals(nv) {};
  };

};

#define FILL_FRAME_WITH_RETURN_REGISTERS(frame,retval) \
  for ( size_t _i = 0, _iEnd(MIN(retval.nvals,LCC_RETURN_VALUES_IN_REGISTERS())); _i<_iEnd; ++_i) { \
    (*frame)[_i] = reinterpret_cast<T_O*>(retval.ret0[_i]); \
  }
  
#define LCC_UNUSED NULL
#define LCC_FIXED_ARGS LCC_ARGS_IN_REGISTERS
#define LCC_FROM_SMART_PTR(x) (x.raw_())
#define LCC_TYPE T_O *
#define LCC_FROM_ACTIVATION_FRAME_SMART_PTR(x) (x.raw_())
#define LCC_TO_SMART_PTR(x) (gctools::smart_ptr<core::T_O>((gc::Tagged)x))

// Pass a defined number of arguments to operator()
#define LCC_PASS_ARGS0_ELLIPSIS(funcRaw)                funcRaw, 0, NULL
#define LCC_PASS_ARGS1_ELLIPSIS(funcRaw,a0)             funcRaw, 1, a0
#define LCC_PASS_ARGS2_ELLIPSIS(funcRaw,a0, a1)         funcRaw, 2, a0,   a1
#define LCC_PASS_ARGS3_ELLIPSIS(funcRaw,a0, a1, a2)     funcRaw, 3, a0,   a1,   a2
#define LCC_PASS_ARGS4_ELLIPSIS(funcRaw,a0, a1, a2, a3) funcRaw, 4, a0,   a1,   a2,   a3

#define LCC_PASS_MAIN() NULL, 0, NULL, NULL, NULL, NULL

// Don't need lcc_arglist because no arguments are passed
#define LCC_PASS_ARGS0_VASLIST(funcRaw) funcRaw, 0, NULL, NULL, NULL, NULL
#define LCC_PASS_ARGS1_VASLIST(funcRaw,a0) funcRaw, 1, a0, NULL, NULL, NULL

#define LCC_UNUSED_rest0 NULL, NULL, NULL, NULL

// To invoke "invoke" methods use these

#define LCC_ARGS_FUNCALL_ELLIPSIS core::T_O *lcc_closure, std::size_t lcc_nargs, ...
#define LCC_ARGS_FUNCALL_VASLIST core::T_O *lcc_closure, std::size_t lcc_nargs, vaslist lcc_args
#define LCC_ARGS_CC_CALL_ELLIPSIS core::T_O *lcc_closure, std::size_t lcc_nargs, ...
#define LCC_ARGS_ELLIPSIS core::T_O *lcc_closure, std::size_t lcc_nargs, ...
#define LCC_ARGS_LLH std::size_t lcc_nargs, core::T_O** lcc_args 
// When you pass args to another function use LCC_PASS_ARGS
#define LCC_PASS_ARGS lcc_closure, lcc_nargs
#define LCC_PASS_ARGS_LLH lcc_nargs, lcc_args

/*! This is a void function */
#define LISP_ENTRY_0() LCC_RETURN entry_point_0(core::T_O *lcc_closure)
#define LISP_ENTRY_1() LCC_RETURN entry_point_1(core::T_O *lcc_closure, core::T_O* lcc_farg0)
#define LISP_ENTRY_2() LCC_RETURN entry_point_2(core::T_O *lcc_closure, core::T_O* lcc_farg0, core::T_O* lcc_farg1 )
#define LISP_ENTRY_3() LCC_RETURN entry_point_3(core::T_O *lcc_closure, core::T_O* lcc_farg0, core::T_O* lcc_farg1, core::T_O* lcc_farg2 )
#define LISP_ENTRY_4() LCC_RETURN entry_point_4(core::T_O *lcc_closure, core::T_O* lcc_farg0, core::T_O* lcc_farg1, core::T_O* lcc_farg2, core::T_O* lcc_farg3 )
#define LISP_ENTRY_5() LCC_RETURN entry_point_5(core::T_O *lcc_closure, core::T_O* lcc_farg0, core::T_O* lcc_farg1, core::T_O* lcc_farg2, core::T_O* lcc_farg3, core::T_O* lcc_farg4 )

// Compiled functions get the raw vaslist
#define LCC_VASLIST(_valist) (*_valist)._Args
#define LCC_VA_START_ARG lcc_nargs


#define LCC_CLOSED_ENVIRONMENT core::T_O *lcc_closedEnvironment

#define LCC_VIRTUAL virtual
#define LCC_RETURN gc::return_type

// Return raw values that can be used to construct a core::T_mv
#define LCC_RETURN_RAW gctools::return_type


extern "C" {

LCC_RETURN_RAW general_entry_point_redirect_0(core::T_O* closure );

LCC_RETURN_RAW general_entry_point_redirect_1(core::T_O* closure, core::T_O* farg0 );

LCC_RETURN_RAW general_entry_point_redirect_2(core::T_O* closure, core::T_O* farg0, core::T_O* farg1 );

LCC_RETURN_RAW general_entry_point_redirect_3(core::T_O* closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2 );

LCC_RETURN_RAW general_entry_point_redirect_4(core::T_O* closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2, core::T_O* farg3 );

LCC_RETURN_RAW general_entry_point_redirect_5(core::T_O* closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2, core::T_O* farg3,  core::T_O* farg4 );

LCC_RETURN_RAW general_entry_point_redirect_6(core::T_O* closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2, core::T_O* farg3,  core::T_O* farg4, core::T_O* farg5 );

LCC_RETURN_RAW general_entry_point_redirect_7(core::T_O* closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2, core::T_O* farg3,  core::T_O* farg4, core::T_O* farg5, core::T_O* farg6 );

};



// To invoke functions of type InitFnPtr use these
#define LCC_PASS_ARGS0_VASLIST_INITFNPTR() nil<core::T_O>().raw_(), 0, LCC_UNUSED_rest0

#define MULTIPLE_VALUES_ARRAY core::lisp_multipleValues()

//#define MULTIPLE_VALUES_SETUP() core::T_sp* __multipleValuesPtr = core::lisp_multipleValues().start_address()

  
/*! This is X86_64 dependent code */
#if defined(X86_64) && defined(_ADDRESS_MODEL_64)

// This is VERY HACKISH
// it's based on the System V Application Binary Interface for X86_64
// I'm writing the register arguments into the reg_save_area and then
// resetting the gp_offset to point to the first register argument lcc_fixed_arg0
#define LCC_ABI_ARGS_IN_REGISTERS 6

#define ASSERT_LCC_VASLIST_AT_START(_valist_s_) \
  ASSERT((_valist_s_)._Args->gp_offset == sizeof(uintptr_t) * (LCC_ABI_ARGS_IN_REGISTERS - LCC_ARGS_IN_REGISTERS));

// Registers are %rdi, %rsi, %rdx, %rcx, %r8, %r9
#define LCC_CLOSURE_REGISTER 0
#define LCC_NARGS_REGISTER 1
#define LCC_ARGS_PTR_REGISTER 2
#define LCC_ARGS_PASSED_IN_REGISTERS LCC_ARGS_IN_REGISTERS
#define LCC_TOTAL_REGISTERS LCC_ABI_ARGS_IN_REGISTERS
#define LCC_ARG0_REGISTER 2
#define LCC_SPILL_NUMBER_ARGUMENTS_TO_VASLIST(_valist_s_, _num_)                               \
  {                                                                                            \
    ((uintptr_t *)(_valist_s_)._Args->reg_save_area)[LCC_NARGS_REGISTER] = (uintptr_t)(_num_); \
  }
#define LCC_SPILL_CLOSURE_TO_VASLIST(_valist_s_,_closure_)    ((core::T_O* *)(_valist_s_)._Args->reg_save_area)[LCC_CLOSURE_REGISTER] = (core::T_O*)_closure_;

#define LCC_REGISTER_SAVE_AREA_CLOSURE(rsa) ((core::T_O**)rsa[LCC_CLOSURE_REGISTER])

#define private_LCC_VASLIST_TOTAL_NUMBER_OF_ARGUMENTS(_args) (size_t)(((uintptr_t *)(_args[0].reg_save_area))[LCC_NARGS_REGISTER])
#define private_LCC_VASLIST_SET_TOTAL_NUMBER_OF_ARGUMENTS(_args, _n) (((uintptr_t *)(_args[0].reg_save_area))[LCC_NARGS_REGISTER]) = ((uintptr_t)_n)
//#define private_LCC_VASLIST_DECREMENT_TOTAL_NUMBER_OF_ARGUMENTS(_args) (--((uintptr_t *)(_args[0].reg_save_area))[LCC_NARGS_REGISTER])

#define LCC_VASLIST_CLOSURE(_args) core::Function_sp((gctools::Tagged)((core::T_O **)(*_args)._Args->reg_save_area)[LCC_CLOSURE_REGISTER])
#define LCC_VASLIST_REGISTER_SAVE_AREA(_args) (core::T_O **)(((*_args))[0].reg_save_area)
#define LCC_VASLIST_OVERFLOW_ARG_AREA(_args) (core::T_O **)(((*_args))[0].overflow_arg_area)
//#define LCC_ORIGINAL_VASLIST_OVERFLOW_ARG_AREA(_args) ((core::T_O***)LCC_VASLIST_REGISTER_SAVE_AREA(_args))[LCC_OVERFLOW_SAVE_REGISTER]
#define LCC_VASLIST_TOTAL_NUMBER_OF_ARGUMENTS(_args) private_LCC_VASLIST_TOTAL_NUMBER_OF_ARGUMENTS((*_args)._Args)
#define LCC_VASLIST_SET_TOTAL_NUMBER_OF_ARGUMENTS(_args, _n) private_LCC_VASLIST_SET_TOTAL_NUMBER_OF_ARGUMENTS((*_args)._Args, _n)
//#define LCC_VASLIST_DECREMENT_TOTAL_NUMBER_OF_ARGUMENTS(_args) private_LCC_VASLIST_DECREMENT_TOTAL_NUMBER_OF_ARGUMENTS((*_args)._Args)

/* Return the indexed argument in the vaslist wrt the remaining arguments in the list.
   This means that if va_arg has already been called on this vaslist  then the 0th
   entry is the next argument that would be returned by vaslist */
#define LCC_VASLIST_RELATIVE_INDEXED_ARG(_res, _args, _idx) { \
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
  LCC_SPILL_CLOSURE_TO_VASLIST(valist_s,_closure.raw_());                \
  size_t lcc_nargs = (_frame).number_of_arguments();                    \
  core::T_O *lcc_arglist = valist_s.asTaggedPtr();                      \
  switch (lcc_nargs) {                                                  \
  default:                                                              \
      _result = _closure->invoke_vaslist(LCC_PASS_ARGS_ELLIPSIS_GENERAL(_closure.raw_(), lcc_nargs, (_frame)[0], (_frame)[1], (_frame)[2], (_frame)[3])); \
    break;                                                              \
  case 4:                                                               \
      _result = _closure->invoke_vaslist(LCC_PASS_ARGS4_ELLIPSIS(_closure.raw_(),(_frame)[0], (_frame)[1], (_frame)[2], (_frame)[3])); \
    break;                                                              \
  case 3:                                                               \
      _result = _closure->invoke_vaslist(LCC_PASS_ARGS3_ELLIPSIS(_closure.raw_(),(_frame)[0], (_frame)[1], (_frame)[2])); \
    break;                                                              \
  case 2:                                                               \
      _result = _closure->invoke_vaslist(LCC_PASS_ARGS2_ELLIPSIS(_closure.raw_(),(_frame)[0], (_frame)[1])); \
    break;                                                              \
  case 1:                                                               \
      _result = _closure->invoke_vaslist(LCC_PASS_ARGS1_ELLIPSIS(_closure.raw_(),(_frame)[0])); \
    break;                                                              \
  case 0:                                                               \
      _result = _closure->invoke_vaslist(LCC_PASS_ARGS0_ELLIPSIS(_closure.raw_())); \
    break;                                                              \
  };


#define LCC_DECLARE_VASLIST()                           \
  Vaslist lcc_arglist_struct(lcc_nargs);               \
  va_start(lcc_arglist_struct._Args, LCC_VA_START_ARG); \
  Vaslist_sp lcc_arglist(&lcc_arglist_struct);

#define LCC_SETUP_VASLIST_FROM_VASLIST(_dest_,_src_) va_copy(_dest_,_src_)

// Create a Vaslist_sp from lcc_vargs (a vaslist)
//#define LCC_MAKE_VASLIST_SP(valist_sp) Vaslist_sp valist_sp(lcc_nargs,lcc_vargs);
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

typedef LCC_RETURN_RAW(*ClaspLocalFunction)();
typedef LCC_RETURN_RAW(*ClaspXepAnonymousFunction)();
#define LISP_CALLING_CONVENTION() entry_point_n(core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args )
typedef LCC_RETURN_RAW(*ClaspXepGeneralFunction)(core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args );
typedef LCC_RETURN_RAW(*ClaspXep0Function)(core::T_O* lcc_closure);
typedef LCC_RETURN_RAW(*ClaspXep1Function)(core::T_O* lcc_closure, core::T_O* farg0);
typedef LCC_RETURN_RAW(*ClaspXep2Function)(core::T_O* lcc_closure, core::T_O* farg0, core::T_O* farg1 );
typedef LCC_RETURN_RAW(*ClaspXep3Function)(core::T_O* lcc_closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2 );
typedef LCC_RETURN_RAW(*ClaspXep4Function)(core::T_O* lcc_closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2, core::T_O* farg3 );
typedef LCC_RETURN_RAW(*ClaspXep5Function)(core::T_O* lcc_closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2, core::T_O* farg3, core::T_O* farg4 );

struct XepFilling {};
struct XepFillUsingLambda {};

struct ClaspXepFunction {
  static const int Entries = NUMBER_OF_ENTRY_POINTS;
  ClaspXepAnonymousFunction _EntryPoints[NUMBER_OF_ENTRY_POINTS];
  bool _Defined;
  ClaspXepFunction() : _Defined(false) {};
  //! Use this ctor when filling ClaspXepFunction with entry points
  ClaspXepFunction(XepFilling f) : _Defined(true) {};
  template <typename Wrapper>
  void setup() {
    assert(NUMBER_OF_ENTRY_POINTS==7);
    this->_Defined = true;
    this->_EntryPoints[0] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_n;
    this->_EntryPoints[1] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_0;
    this->_EntryPoints[2] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_1;
    this->_EntryPoints[3] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_2;
    this->_EntryPoints[4] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_3;
    this->_EntryPoints[5] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_4;
    this->_EntryPoints[6] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_5;
  }
  ClaspXepAnonymousFunction operator[](int index) const { return this->_EntryPoints[index]; };
  inline LCC_RETURN invoke_0(T_O* closure) {
    return ((ClaspXep0Function)(this->_EntryPoints[1]))( closure );
  }
  inline LCC_RETURN invoke_1(T_O* closure, T_O* farg0) {
    return ((ClaspXep1Function)(this->_EntryPoints[2]))( closure, farg0 );
  }
};

typedef LCC_RETURN_RAW (*fnLispCallingConvention)(LCC_ARGS_ELLIPSIS);
typedef LCC_RETURN_RAW (*CompiledClosure_fptr_type)(LCC_ARGS_ELLIPSIS);
typedef LCC_RETURN (*InitFnPtr)(LCC_ARGS_ELLIPSIS);
typedef LCC_RETURN (*DispatchFunction_fptr_type)(gctools::Tagged gf, gctools::Tagged valist_sptr);
typedef LCC_RETURN (*ShutdownFunction_fptr_type)();
#endif


#ifdef LCC_FUNCALL

extern "C" {

std::string dbg_safe_repr(uintptr_t raw);

// Return true if the Vaslist is at the head of the list and false if it is used up
inline bool dump_Vaslist_ptr(FILE* fout, Vaslist* args) {
  fprintf(fout,"Vaslist dump @%p\n", (void*)args);
  fprintf(fout,"Vaslist nargs = %lu\n", args->_nargs);
  int iEnd = args->_nargs;
  if (iEnd>CALL_ARGUMENTS_LIMIT) {
    iEnd = 0;
    fprintf(fout,"%s:%d      args->_nargs -> %lu !!!!  A BAD VALUE\n", __FILE__, __LINE__, args->_nargs);
  }
  for ( int i=0; i<iEnd; ++i ) {
    T_O* arg = (*args)[i];
    fprintf(fout,"     [%d] --> %p %s\n", i, arg, dbg_safe_repr((uintptr_t)arg).c_str() );
  }
  return true;
};
};


/*! Call a function object with args in a Vaslist_sp and consume the valist.
The Callee can NOT use args after this call.
Note: Since we don't have the full Function_O class definition when this
header is compiled (and we don't want to #include it because of all the problems
that will cause) I made this a template function where you pass the function
type as a template argument.  Call it like this...
funcall_consume_valist_<core::Function_O>(tagged_func_ptr,valist_args)
*/
template <typename Func_O_Type>
inline gctools::return_type funcall_general(gc::Tagged func_tagged, size_t nargs, core::T_O** args ) {
  ASSERT(gc::tagged_generalp(func_tagged));
  gc::smart_ptr<Function_O> func((gc::Tagged)func_tagged);
  return func->entry()((core::T_O*)func_tagged,nargs,args);
}

template <typename Func_O_Type>
inline gctools::return_type funcall_vaslist(gc::Tagged func_tagged, Vaslist_sp var ) {
  ASSERT(gc::tagged_generalp(func_tagged));
  gc::smart_ptr<Function_O> func((gc::Tagged)func_tagged);
  return func->entry()((core::T_O*)func_tagged,var->remaining_nargs(), var->args());
}

#endif // LCC_FUNCALL
