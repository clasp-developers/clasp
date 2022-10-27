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
// Return raw values that can be used to construct a core::T_mv
#define LCC_RETURN_RAW gctools::return_type
#define LCC_RETURN gctools::return_type

// To invoke "invoke" methods use these

/*! This is a void function */
#define LISP_ENTRY_0() LCC_RETURN entry_point_0(core::T_O *lcc_closure)
#define LISP_ENTRY_1() LCC_RETURN entry_point_1(core::T_O *lcc_closure, core::T_O* lcc_farg0)
#define LISP_ENTRY_2() LCC_RETURN entry_point_2(core::T_O *lcc_closure, core::T_O* lcc_farg0, core::T_O* lcc_farg1 )
#define LISP_ENTRY_3() LCC_RETURN entry_point_3(core::T_O *lcc_closure, core::T_O* lcc_farg0, core::T_O* lcc_farg1, core::T_O* lcc_farg2 )
#define LISP_ENTRY_4() LCC_RETURN entry_point_4(core::T_O *lcc_closure, core::T_O* lcc_farg0, core::T_O* lcc_farg1, core::T_O* lcc_farg2, core::T_O* lcc_farg3 )
#define LISP_ENTRY_5() LCC_RETURN entry_point_5(core::T_O *lcc_closure, core::T_O* lcc_farg0, core::T_O* lcc_farg1, core::T_O* lcc_farg2, core::T_O* lcc_farg3, core::T_O* lcc_farg4 )

template <typename WRAPPER, typename...ARGS>
inline LCC_RETURN arity_entry_point(core::T_O* lcc_closure, ARGS... lcc_arg)
{
  if constexpr (WRAPPER::NumParams == sizeof...(ARGS)) {
      core::T_O* args[sizeof...(ARGS)] = {lcc_arg...};
      return WRAPPER::entry_point_n(lcc_closure, lcc_arg... );
    }
  cc_wrong_number_of_arguments( lcc_closure, sizeof...(ARGS), WRAPPER::NumParams, WRAPPER::NumParams );
}


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

#define MULTIPLE_VALUES_ARRAY core::lisp_multipleValues()


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

// Create a Vaslist_sp from lcc_vargs (a vaslist)
//#define LCC_MAKE_VASLIST_SP(valist_sp) Vaslist_sp valist_sp(lcc_nargs,lcc_vargs);
#endif
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
typedef LCC_RETURN_RAW(*BytecodeTrampolineFunction)(unsigned char* pc, core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args );
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
  int _RequiredArgs;
  bool _Defined;
  ClaspXepFunction() : _RequiredArgs(-1), _Defined(false) {};
  //! Use this ctor when filling ClaspXepFunction with entry points
  ClaspXepFunction(XepFilling f) : _RequiredArgs(-1), _Defined(true) {};
  template <typename Wrapper>
  static ClaspXepFunction make() {
    assert(NUMBER_OF_ENTRY_POINTS==7);
    ClaspXepFunction me;
    me._Defined = true;
    me._EntryPoints[0] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_n;
    me._EntryPoints[1] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_0;
    me._EntryPoints[2] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_1;
    me._EntryPoints[3] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_2;
    me._EntryPoints[4] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_3;
    me._EntryPoints[5] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_4;
    me._EntryPoints[6] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_5;
    return me;
  }
  template <typename Wrapper>
  static ClaspXepFunction make(size_t specializer_length) {
    assert(NUMBER_OF_ENTRY_POINTS==7);
    ClaspXepFunction me;
    me._RequiredArgs = specializer_length;
    me._Defined = true;
    me._EntryPoints[0] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_n;

    if (0<specializer_length)
      me._EntryPoints[1] = (ClaspXepAnonymousFunction)&Wrapper::error_entry_point_0;
    else me._EntryPoints[1] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_0;

    if (1<specializer_length)
      me._EntryPoints[2] = (ClaspXepAnonymousFunction)&Wrapper::error_entry_point_1;
    else       me._EntryPoints[2] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_1;

    if (2<specializer_length)
      me._EntryPoints[3] = (ClaspXepAnonymousFunction)&Wrapper::error_entry_point_2;
    else me._EntryPoints[3] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_2;

    if (3<specializer_length)
      me._EntryPoints[4] = (ClaspXepAnonymousFunction)&Wrapper::error_entry_point_3;
    else me._EntryPoints[4] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_3;

    if (4<specializer_length)
      me._EntryPoints[5] = (ClaspXepAnonymousFunction)&Wrapper::error_entry_point_4;
    else me._EntryPoints[5] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_4;

    if (5<specializer_length)
      me._EntryPoints[6] = (ClaspXepAnonymousFunction)&Wrapper::error_entry_point_5;
    else me._EntryPoints[6] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_5;

    return me;
  }
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
  void fixupInternalsForSnapshotSaveLoad(CodeSimpleFun_O* cep, snapshotSaveLoad::Fixup* fixup) {
    printf("%s:%d:%s See function.h/clbind/line136\n", __FILE__, __LINE__, __FUNCTION__ );
  }
  ClaspXepAnonymousFunction operator[](int index) const { return this->_EntryPoints[index]; };

  inline LCC_RETURN invoke_n(T_O* closure,size_t lcc_nargs, T_O** lcc_args) const {
    return ((ClaspXepGeneralFunction)(this->_EntryPoints[0]))( closure, lcc_nargs, lcc_args );
  }

  inline LCC_RETURN invoke_0(T_O* closure) const {
    return ((ClaspXep0Function)(this->_EntryPoints[1]))( closure );
  }

  inline LCC_RETURN invoke_1(T_O* closure, T_O* farg0) const {
    return ((ClaspXep1Function)(this->_EntryPoints[2]))( closure, farg0 );
  }

  inline LCC_RETURN invoke_2(T_O* closure, T_O* farg0, T_O* farg1 ) const {
    return ((ClaspXep2Function)(this->_EntryPoints[3]))( closure, farg0, farg1 );
  }

  inline LCC_RETURN invoke_3(T_O* closure, T_O* farg0, T_O* farg1, T_O* farg2 ) const {
    return ((ClaspXep3Function)(this->_EntryPoints[4]))( closure, farg0, farg1, farg2 );
  }

  inline LCC_RETURN invoke_4(T_O* closure, T_O* farg0, T_O* farg1, T_O* farg2, T_O* farg3 ) const {
    return ((ClaspXep4Function)(this->_EntryPoints[5]))( closure, farg0, farg1, farg2, farg3 );
  }

  inline LCC_RETURN invoke_5(T_O* closure, T_O* farg0, T_O* farg1, T_O* farg2, T_O* farg3, T_O* farg4 ) const {
    return ((ClaspXep5Function)(this->_EntryPoints[6]))( closure, farg0, farg1, farg2, farg3, farg4 );
  }

};

#endif


#ifdef LCC_FUNCALL

extern "C" {

std::string dbg_safe_repr(uintptr_t raw);

// Return true if the Vaslist is at the head of the list and false if it is used up
inline bool dump_Vaslist_ptr(FILE* fout, Vaslist* args) {
  fprintf(fout,"Vaslist dump @%p\n", (void*)args);
  fprintf(fout,"Vaslist nargs = %lu\n", args->nargs());
  int iEnd = args->nargs();
  if (iEnd>CALL_ARGUMENTS_LIMIT) {
    iEnd = 0;
    fprintf(fout,"%s:%d      args->_nargs -> %lu !!!!  A BAD VALUE\n", __FILE__, __LINE__, args->nargs());
  }
  for ( int i=0; i<iEnd; ++i ) {
    T_O* arg = (*args)[i];
    fprintf(fout,"     [%d] --> %p %s\n", i, arg, dbg_safe_repr((uintptr_t)arg).c_str() );
  }
  return true;
};

inline void dump_lcc_args(FILE* fout, size_t lcc_nargs, T_O** lcc_args) {
  fprintf(fout,"lcc_args dump @%p\n", (void*)lcc_args);
  fprintf(fout,"lcc_nargs = %zu", lcc_nargs);
  if (lcc_nargs > CALL_ARGUMENTS_LIMIT) {
    fprintf(fout,"%s:%d      args->_nargs -> %zu !!!!  A BAD VALUE\n", __FILE__, __LINE__, lcc_nargs);
  } else {
    for (size_t i = 0; i < lcc_nargs; ++i) {
      T_O* arg = lcc_args[i];
      fprintf(fout,"     [%zu] --> %p %s\n", i, arg, dbg_safe_repr((uintptr_t)arg).c_str() );
    }
  }
}

}; // extern "C"


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

#endif // LCC_FUNCALL
