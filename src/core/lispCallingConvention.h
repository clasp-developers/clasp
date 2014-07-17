
#define LISP_CALLING_CONVENTION() void activate( core::T_mv* lcc_resultP, int lcc_nargs, core::T_sp lcc_fixed_arg0, core::T_sp lcc_fixed_arg1, core::T_sp lcc_fixed_arg2, ... )
#define LCC_ARG0() lcc_fixed_arg0
#define LCC_ARG1() lcc_fixed_arg1
#define LCC_ARG2() lcc_fixed_arg2
#define LCC_FIXED_NUM 3
#define LCC_ARG3() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG4() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG5() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG6() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG7() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG8() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG9() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG10() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG11() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG12() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG13() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG14() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG15() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG16() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG17() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG18() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG19() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))
#define LCC_ARG20() gctools::smart_ptr<core::T_O>(va_arg(lcc_arglist,core::T_O*))


#define LCC_VA_START_DONT_USE_VARARGS()
#define LCC_VA_END_DONT_USE_VARARGS()

#define LCC_VA_START_USE_VARARGS() va_list lcc_arglist; va_start(lcc_arglist, lcc_fixed_arg2);
#define LCC_VA_END_USE_VARARGS() va_end(lcc_arglist);

/* These are empty because the varargs aren't needed for 0-3 arguments */
#define LCC_VA_START0() LCC_VA_START_DONT_USE_VARARGS()
#define LCC_VA_START1() LCC_VA_START_DONT_USE_VARARGS()
#define LCC_VA_START2() LCC_VA_START_DONT_USE_VARARGS()
#define LCC_VA_START3() LCC_VA_START_DONT_USE_VARARGS()

#define LCC_VA_END0() LCC_VA_END_DONT_USE_VARARGS()
#define LCC_VA_END1() LCC_VA_END_DONT_USE_VARARGS()
#define LCC_VA_END2() LCC_VA_END_DONT_USE_VARARGS()
#define LCC_VA_END3() LCC_VA_END_DONT_USE_VARARGS()

/* These do need varargs */
#define LCC_VA_START4() LCC_VA_START_USE_VARARGS()
#define LCC_VA_START5() LCC_VA_START_USE_VARARGS()
#define LCC_VA_START6() LCC_VA_START_USE_VARARGS()
#define LCC_VA_START7() LCC_VA_START_USE_VARARGS()
#define LCC_VA_START8() LCC_VA_START_USE_VARARGS()
#define LCC_VA_START9() LCC_VA_START_USE_VARARGS()
#define LCC_VA_START10() LCC_VA_START_USE_VARARGS()
#define LCC_VA_START11() LCC_VA_START_USE_VARARGS()
#define LCC_VA_START12() LCC_VA_START_USE_VARARGS()
#define LCC_VA_START13() LCC_VA_START_USE_VARARGS()
#define LCC_VA_START14() LCC_VA_START_USE_VARARGS()
#define LCC_VA_START15() LCC_VA_START_USE_VARARGS()
#define LCC_VA_START16() LCC_VA_START_USE_VARARGS()

#define LCC_VA_END4() LCC_VA_END_USE_VARARGS()
#define LCC_VA_END5() LCC_VA_END_USE_VARARGS()
#define LCC_VA_END6() LCC_VA_END_USE_VARARGS()
#define LCC_VA_END7() LCC_VA_END_USE_VARARGS()
#define LCC_VA_END8() LCC_VA_END_USE_VARARGS()
#define LCC_VA_END9() LCC_VA_END_USE_VARARGS()
#define LCC_VA_END10() LCC_VA_END_USE_VARARGS()
#define LCC_VA_END11() LCC_VA_END_USE_VARARGS()
#define LCC_VA_END12() LCC_VA_END_USE_VARARGS()
#define LCC_VA_END13() LCC_VA_END_USE_VARARGS()
#define LCC_VA_END14() LCC_VA_END_USE_VARARGS()
#define LCC_VA_END15() LCC_VA_END_USE_VARARGS()
#define LCC_VA_END16() LCC_VA_END_USE_VARARGS()

