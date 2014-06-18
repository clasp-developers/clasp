/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    legacy.h -- Legacy macros, functions and names.
*/
/*
    Copyright (c) 2011, Juan Jose Garcia-Ripoll

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#if !defined(ECL_LEGACY_H) && !defined(ECL_NO_LEGACY)
#define ECL_LEGACY_H

/*
 * LEGACY
 */
#define CHARACTERP(o)		ECL_CHARACTERP(o)
#define BASE_CHAR_P(o)		ECL_BASE_CHAR_P(o)
#define BASE_CHAR_CODE_P(o)	ECL_BASE_CHAR_CODE_P(o)
#define CODE_CHAR(o)		ECL_CODE_CHAR(o)
#define CHAR_CODE(o)		ECL_CHAR_CODE(o)
#define REAL_TYPE(t)		ECL_REAL_TYPE_P(t)
#define IMMEDIATE(o)		ECL_IMMEDIATE(o)
#define IMMEDIATE_TAG		ECL_IMMEDIATE_TAG

#define FIXNUM_TAG		t_fixnum
#define MAKE_FIXNUM(n)		ecl_make_fixnum(n)
#define FIXNUM_MINUSP(n)	ecl_fixnum_minusp(n)
#define FIXNUM_PLUSP(n)		ecl_fixnum_plusp(n)
#define	fix(o)			ecl_fixnum(o)
#define FIXNUMP(o)		ECL_FIXNUMP(o)

#define	sf(obje)	(obje)->SF.SFVAL
#define	df(obje)	(obje)->DF.DFVAL
#define make_shortfloat(x) ecl_make_shortfloat(x);

#define big_dim		big_num->_mp_alloc
#define big_size	big_num->_mp_size
#define big_limbs	big_num->_mp_d

#define cl_def_c_function_va(sym,function) ecl_def_c_function_va(sym,function)
#define cl_def_c_function(sym,function,narg) ecl_def_c_function(sym,function,narg)
#define cl_def_c_macro(sym,function,narg) {                     \
                int n = (narg);                                 \
                if (n < 0)                                      \
                        ecl_def_c_macro_va((sym),(function));   \
                else                                            \
                        ecl_def_c_macro((sym),(function),n); }
#define cl_make_cfun(fun,name,block,narg) ecl_make_cfun(fun,name,block,narg)
#define cl_make_cfun_va(fun,name,block) ecl_make_cfun_va(fun,name,block)
#define cl_make_cclosure_va(fun,name,block) ecl_make_cclosure_va(fun,name,block)
#define si_bc_file(o) si_compiled_function_file(o)
#define ARRAYP ECL_ARRAYP
#define VECTORP ECL_VECTORP
#define ARRAY_TYPE(t) (t >= t_array && t <= t_bitvector)
#define c_string_to_object ecl_read_from_cstring

#define big_register0_get _ecl_big_register0
#define big_register1_get _ecl_big_register1
#define big_register2_get _ecl_big_register2
#define big_register_free _ecl_big_register_free
#define big_register_copy _ecl_big_register_copy
#define big_register_normalize _ecl_big_register_normalize
/* #define big_copy _ecl_big_copy  Has disappeared */
/* #define big_to_double Has disappeared */

#define cl_alloc_simple_base_string ecl_alloc_simple_base_string
#define cl_alloc_adjustable_base_string ecl_alloc_adjustable_base_string
#define cl_alloc_simple_extended_string ecl_alloc_simple_extended_string

#define ecl_search_hash _ecl_gethash

#define read_VV ecl_init_module

#endif /* !ECL_LEGACY_H && !ECL_NO_LEGACY */

#define make_simple_base_string(s) ecl_make_simple_base_string((s),-1)
#define make_constant_base_string(s) ecl_make_simple_base_string((char *)(s),-1)

#define stp_ordinary ecl_stp_ordinary
#define stp_constant ecl_stp_constant
#define stp_special ecl_stp_special
#define stp_macro ecl_stp_macro
#define stp_special_form ecl_stp_special_form

#define aet_object ecl_aet_object
#define aet_sf ecl_aet_sf
#define aet_df ecl_aet_df
#define aet_bit ecl_aet_bit
#define aet_fix ecl_aet_fix
#define aet_index ecl_aet_index
#define aet_b8 ecl_aet_b8
#define aet_i8 ecl_aet_i8
#define aet_b16 ecl_aet_b16
#define aet_i16 ecl_aet_i16
#define aet_b32 ecl_aet_b32
#define aet_i32 ecl_aet_i32
#define aet_b64 ecl_aet_b64
#define aet_i64 ecl_aet_i64
#define aet_ch ecl_aet_ch
#define aet_bc ecl_aet_bc

#define htt_eq ecl_htt_eq
#define htt_eql ecl_htt_eql
#define htt_equal ecl_htt_equal
#define htt_equalp ecl_htt_equalp
#define htt_not_weak ecl_htt_not_weak
#define htt_weak_key ecl_htt_weak_key
#define htt_weak_value ecl_htt_weak_value
#define htt_weak_key_and_value ecl_htt_weak_key_and_value

#define ecl_make_singlefloat ecl_make_single_float
#define ecl_make_doublefloat ecl_make_double_float
#define ecl_make_longfloat ecl_make_long_float

#define number_to_float(x) ((float)ecl_to_double(x))

#define ecl_make_unsigned_long_Long(o) ecl_make_ulong_long(o)
#define ecl_to_unsigned_long_long(o) ecl_to_ulong_long(o)

#define ADIMLIM ECL_ARRAY_DIMENSION_LIMIT
#define ATOTLIM ECL_ARRAY_TOTAL_LIMIT
#define ARANKLIM ECL_ARRAY_RANK_LIMIT

#define CALL_ARGUMENTS_LIMIT ECL_CALL_ARGUMENTS_LIMIT
#define LAMBDA_PARAMETERS_LIMIT ECL_LAMBDA_PARAMETERS_LIMIT
#define C_ARGUMENTS_LIMIT ECL_C_ARGUMENTS_LIMIT

#define cl_va_start ecl_va_start
#define cl_va_list ecl_va_list
#define cl_va_arg ecl_va_arg
#define cl_va_end ecl_va_end

#define CHAR_CODE_LIMIT ECL_CHAR_CODE_LIMIT

#define NVALUES		cl_env.nvalues
#define VALUES(n)	cl_env.values[n]
#define return0()	return ((NVALUES = 0),ECL_NIL)
#define return1(x)	return ((VALUES(0)=(x)),(NVALUES=1),VALUES(0))
#define returnn(x)	return x

#define CL_UNWIND_PROTECT_BEGIN ECL_UNWIND_PROTECT_BEGIN
#define CL_UNWIND_PROTECT_END ECL_UNWIND_PROTECT_END
#define CL_UNWIND_PROTECT_EXIT ECL_UNWIND_PROTECT_EXIT
#define CL_BLOCK_END ECL_BLOCK_END
#define CL_BLOCK_BEGIN ECL_BLOCK_BEGIN
#define CL_CATCH_BEGIN ECL_CATCH_BEGIN
#define CL_CATCH_END ECL_CATCH_END
#define CL_CATCH_ALL_BEGIN ECL_CATCH_ALL_BEGIN
#define CL_CATCH_ALL_END ECL_CATCH_ALL_END

#define bds_bd ecl_bds_frame
typedef struct ecl_bds_frame *bds_ptr;
typedef struct ecl_ihs_frame *ihs_ptr;

#define LISTP(x)	ECL_LISTP(x)
#define CONSP(x)	ECL_CONSP(x)
#define ATOM(x)		ECL_ATOM(x)
#define SYMBOLP(x)	ECL_SYMBOLP(x)

enum {	/*  stream mode  */
	smm_input,		/*  input  */
	smm_input_file,		/*  input  */
	smm_output,		/*  output  */
	smm_output_file,	/*  output  */
	smm_io,			/*  input-output  */
	smm_io_file,		/*  input-output  */
	smm_synonym,		/*  synonym  */
	smm_broadcast,		/*  broadcast  */
	smm_concatenated,	/*  concatenated  */
	smm_two_way,		/*  two way  */
	smm_echo,		/*  echo  */
	smm_string_input,	/*  string input  */
	smm_string_output,	/*  string output  */
	smm_probe,		/*  probe (only used in open_stream())  */
#if defined(WSOCK)
	smm_input_wsock,	/*  input socket (Win32) */
	smm_output_wsock,	/*  output socket (Win32) */
	smm_io_wsock,		/*  input/output socket (Win32) */
#endif
#if defined(MS_WINDOWS_HOST)
	smm_io_wcon,		/*  windows console (Win32) */
#endif
	smm_sequence_input,	/*  sequence input  */
	smm_sequence_output	/*  sequence output  */
};

#define Cnil ECL_NIL
#define Ct ECL_T

#define SYM_FUN(x) ECL_SYM_FUN(x)

#define CLASS_OF(x) ECL_CLASS_OF(x)
#define CLASS_NAME(x) ECL_CLASS_NAME(x)
#define CLASS_SUPERIORS(x) ECL_CLASS_SUPERIORS(x)
#define CLASS_INFERIORS(x) ECL_CLASS_INFERIORS(x)
#define CLASS_SLOTS(x) ECL_CLASS_SLOTS(x)
#define CLASS_CPL(x) ECL_CLASS_CPL(x)

#define STYPE(x) ECL_STRUCT_TYPE(x)
#define SLOTS(x) ECL_STRUCT_SLOTS(x)
#define SLOT(x,i) ECL_STRUCT_SLOT(x,i)
#define SLENGTH(x) ECL_STRUCT_LENGTH(x)
#define SNAME(x) ECL_STRUCT_NAME(x)

#ifndef type_of
#define type_of(x) ecl_t_of(x)
#endif

