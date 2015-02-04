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
#define LCC_FIXED_ARGS 5
#define LCC_FROM_SMART_PTR(x) (x.asTPtr())
// Pass ActivationFrames as T_O*
#define LCC_FROM_ACTIVATION_FRAME_SMART_PTR(x) (x.asTPtr())
#define LCC_TO_SMART_PTR(x) (gctools::smart_ptr<core::T_O>(x))

#define LCC_UNUSED_1() LCC_UNUSED
#define LCC_UNUSED_2() LCC_UNUSED,LCC_UNUSED
#define LCC_UNUSED_3() LCC_UNUSED,LCC_UNUSED,LCC_UNUSED
#define LCC_UNUSED_4() LCC_UNUSED,LCC_UNUSED,LCC_UNUSED,LCC_UNUSED
#define LCC_UNUSED_5() LCC_UNUSED,LCC_UNUSED,LCC_UNUSED,LCC_UNUSED,LCC_UNUSED
#define LCC_UNUSED_rest4() LCC_UNUSED
#define LCC_UNUSED_rest3() LCC_UNUSED,LCC_UNUSED
#define LCC_UNUSED_rest2() LCC_UNUSED,LCC_UNUSED,LCC_UNUSED
#define LCC_UNUSED_rest1() LCC_UNUSED,LCC_UNUSED,LCC_UNUSED,LCC_UNUSED
#define LCC_UNUSED_rest0() LCC_UNUSED,LCC_UNUSED,LCC_UNUSED,LCC_UNUSED,LCC_UNUSED

#define LCC_PASS_ARGS0() 0, LCC_UNUSED_rest0()
#define LCC_PASS_ARGS1(a0) 1, a0, LCC_UNUSED_rest1()
#define LCC_PASS_ARGS2(a0,a1) 2, a0, a1, LCC_UNUSED_rest2()
#define LCC_PASS_ARGS3(a0,a1,a2) 3, a0, a1, a2, LCC_UNUSED_rest3()


//#define LCC_ARGS_INTRINSICS size_t lcc_nargs, core::T_O* lcc_fixed_arg0, core::T_O* lcc_fixed_arg1, core::T_O* lcc_fixed_arg2, core::T_O* lcc_fixed_arg3, core::T_O* lcc_fixed_arg4

#define LCC_ARGS_BASE std::size_t lcc_nargs, core::T_O* lcc_fixed_arg0, core::T_O* lcc_fixed_arg1, core::T_O* lcc_fixed_arg2, core::T_O* lcc_fixed_arg3, core::T_O* lcc_fixed_arg4
#define LCC_ARGS_ELIPSIS LCC_ARGS_BASE
#define LCC_ARGS LCC_ARGS_BASE
// When you pass args to another function use LCC_PASS_ARGS
#define LCC_PASS_ARGS lcc_nargs,lcc_fixed_arg0,lcc_fixed_arg1,lcc_fixed_arg2,lcc_fixed_arg3,lcc_fixed_arg4

#define LCC_CLOSED_ENVIRONMENT core::T_O* lcc_closedEnvironment
#define LCC_RETURN core::T_mv* lcc_resultP
//#define LCC_RETURN_ARGS core::T_mv* lcc_resultP, LCC_ARGS



/*! This is a void function */
#define LISP_CALLING_CONVENTION() invoke( LCC_RETURN, LCC_ARGS )


#define MULTIPLE_VALUES_ARRAY core::lisp_multipleValues()

#define LCC_ARG0() gctools::smart_ptr<core::T_O>(lcc_fixed_arg0)
#define LCC_ARG1() gctools::smart_ptr<core::T_O>(lcc_fixed_arg1)
#define LCC_ARG2() gctools::smart_ptr<core::T_O>(lcc_fixed_arg2)
#define LCC_ARG3() gctools::smart_ptr<core::T_O>(lcc_fixed_arg3)
#define LCC_ARG4() gctools::smart_ptr<core::T_O>(lcc_fixed_arg4)
/*! LCC_ARGS_IN_REGISTERS is defined in src/core/config.h and is currently 5 (five)*/
#define LCC_FIXED_NUM LCC_ARGS_IN_REGISTERS
//#define MULTIPLE_VALUES_SETUP() core::T_sp* __multipleValuesPtr = core::lisp_multipleValues().start_address()
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

/* This is a switch statement that copies passed arguments in registers into the MultipleValues array */
#define LCC_SWITCH_TO_COPY_PASSED_ARGS_INTO_MULTIPLE_VALUES_ARRAY(_mv)		 \
    _mv = _lisp->callArgs();				\
    _mv->setSize(lcc_nargs);					 \
    switch (lcc_nargs)						 \
    {								 \
    default:							 \
    case 5:							 \
	(*_mv)[4].px_ref() = lcc_fixed_arg4;			 \
    case 4:							 \
	(*_mv)[3].px_ref() = lcc_fixed_arg3;			 \
    case 3:							 \
	(*_mv)[2].px_ref() = lcc_fixed_arg2;			 \
    case 2:							 \
	(*_mv)[1].px_ref() = lcc_fixed_arg1;			 \
    case 1:							 \
	(*_mv)[0].px_ref() = lcc_fixed_arg0;			 \
    case 0:							 \
	break;							 \
    }


