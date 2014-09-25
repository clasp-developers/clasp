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


#define LISP_CALLING_CONVENTION_ARGS_BASE size_t lcc_nargs, core::T_O* lcc_fixed_arg0, core::T_O* lcc_fixed_arg1, core::T_O* lcc_fixed_arg2
#define LISP_CALLING_CONVENTION_ARGS_ELIPSIS LISP_CALLING_CONVENTION_ARGS_BASE, ...
#define LISP_CALLING_CONVENTION_ARGS LISP_CALLING_CONVENTION_ARGS_BASE, va_list lcc_arglist 

#define LISP_CALLING_CONVENTION_CLOSED_ENVIRONMENT core::T_sp* lcc_closedEnvironment
#define LISP_CALLING_CONVENTION_RETURN core::T_mv* lcc_resultP
//#define LISP_CALLING_CONVENTION_RETURN_ARGS core::T_mv* lcc_resultP, LISP_CALLING_CONVENTION_ARGS


/*! This is a void function */
#define LISP_CALLING_CONVENTION() invoke( LISP_CALLING_CONVENTION_RETURN, LISP_CALLING_CONVENTION_ARGS )


#define LCC_ARG0() (gctools::smart_ptr<core::T_O>(lcc_fixed_arg0))
#define LCC_ARG1() (gctools::smart_ptr<core::T_O>(lcc_fixed_arg1))
#define LCC_ARG2() (gctools::smart_ptr<core::T_O>(lcc_fixed_arg2))
#define LCC_FIXED_NUM 3
#define LCC_ARG3() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG4() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG5() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG6() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG7() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG8() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG9() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG10() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG11() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG12() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG13() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG14() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG15() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG16() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG17() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG18() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG19() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
#define LCC_ARG20() (gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*)))
