/* -*- mode: c; c-basic-offset: 8 -*- */
#ifndef ECL_EXTERNAL_H
#define ECL_EXTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif

#define _ECL_ARGS(x) x

/*
 * Per-thread data.
 */

typedef struct cl_env_struct *cl_env_ptr;
struct cl_env_struct {
	/* Flag for disabling interrupts while we call C library functions. */
	volatile int disable_interrupts;

	/* Array where values are returned by functions. */
	cl_index nvalues;
	cl_object values[ECL_MULTIPLE_VALUES_LIMIT];

        /* Environment for calling closures, CLOS generic functions, etc */
        cl_object function;

	/* The four stacks in ECL. */

	/*
	 * The lisp stack, which is used mainly for keeping the arguments of a
	 * function before it is invoked, and also by the compiler and by the
	 * reader when they are building some data structure.
	 */
	cl_index stack_size;
	cl_object *stack;
	cl_object *stack_top;
	cl_object *stack_limit;

	/*
	 * The BinDing Stack stores the bindings of special variables.
	 */
#ifdef ECL_THREADS
        cl_index thread_local_bindings_size;
        cl_object *thread_local_bindings;
	cl_object bindings_array;
#endif
	cl_index bds_size;
	struct ecl_bds_frame *bds_org;
	struct ecl_bds_frame *bds_top;
	struct ecl_bds_frame *bds_limit;

	/*
	 * The Invocation History Stack (IHS) keeps a list of the names of the
	 * functions that are invoked, together with their lexical
	 * environments.
	 */
	struct ecl_ihs_frame *ihs_top;

	/*
	 * The FRames Stack (FRS) is a list of frames or jump points, and it
	 * is used by different high-level constructs (BLOCK, TAGBODY, CATCH...)
	 * to set return points.
	 */
	cl_index frs_size;
	struct ecl_frame *frs_org;
	struct ecl_frame *frs_top;
	struct ecl_frame *frs_limit;
	struct ecl_frame *nlj_fr;
        cl_index frame_id;

	/*
	 * The following pointers to the C Stack are used to ensure that a
	 * recursive function does not enter an infinite loop and exhausts all
	 * memory. They will eventually disappear, because most operating
	 * systems already take care of this.
	 */
	char *cs_org;
	char *cs_limit;
	char *cs_barrier;
	cl_index cs_size;

	/* Private variables used by different parts of ECL: */
	/* ... the reader ... */
	cl_object string_pool;

	/* ... the compiler ... */
	struct cl_compiler_env *c_env;

	/* ... the formatter ... */
	cl_object fmt_aux_stream;

	/* ... arithmetics ... */
	/* Note: if you change the size of these registers, change also
	   BIGNUM_REGISTER_SIZE in config.h */
	cl_object big_register[3];

	cl_object own_process;
	cl_object pending_interrupt;
	cl_object signal_queue;
	cl_object signal_queue_spinlock;
        void *default_sigmask;

	/* The following is a hash table for caching invocations of
	   generic functions. In a multithreaded environment we must
	   queue operations in which the hash is cleared from updated
	   generic functions. */
#ifdef CLOS
	struct ecl_cache *method_cache;
	struct ecl_cache *slot_cache;
#endif

	/* foreign function interface */
#ifdef HAVE_LIBFFI
        cl_index ffi_args_limit;
	struct _ffi_type **ffi_types;
        union ecl_ffi_values *ffi_values;
        union ecl_ffi_values **ffi_values_ptrs;
#endif
#ifdef ECL_DYNAMIC_FFI
        void *fficall;
#endif

	/* Alternative stack for processing signals */
	void *altstack;
	cl_index altstack_size;

        /* Floating point interrupts which are trapped */
        int trap_fpe_bits;

	/* Old exception filter. Needed by windows. */
	void *old_exception_filter;

        /* List of packages interned when loading a FASL but which have
         * to be explicitely created by the compiled code itself. */
	cl_object packages_to_be_created;
        cl_object packages_to_be_created_p;

	/* Segmentation fault address */
	void *fault_address;

#ifdef ECL_THREADS
	int cleanup;
#endif
};

#ifndef __GNUC__
#define __attribute__(x)
#endif
#if defined(ECL_THREADS)
# ifdef WITH___THREAD
#  define cl_env (*cl_env_p)
#  define ecl_process_env() cl_env_p
   extern __thread cl_env_ptr cl_env_p;
# else
#  define cl_env (*ecl_process_env())
   extern ECL_API cl_env_ptr ecl_process_env(void) __attribute__((const));
# endif
#else
# define cl_env (*cl_env_p)
# define ecl_process_env() cl_env_p
  extern ECL_API cl_env_ptr cl_env_p;
#endif

/*
 * Per-process data. Modify main.d accordingly.
 */

struct cl_core_struct {
	cl_object packages;
	cl_object lisp_package;
	cl_object user_package;
	cl_object keyword_package;
	cl_object system_package;
        cl_object ext_package;
#ifdef CLOS
	cl_object clos_package;
# ifdef ECL_CLOS_STREAMS
	cl_object gray_package;
# endif
#endif
	cl_object mp_package;
        cl_object c_package;
	cl_object ffi_package;

	cl_object pathname_translations;
        cl_object library_pathname;

	cl_object terminal_io;
	cl_object null_stream;
	cl_object standard_input;
	cl_object standard_output;
	cl_object error_output;
	cl_object standard_readtable;
	cl_object dispatch_reader;
	cl_object default_dispatch_macro;

	cl_object char_names;
	cl_object null_string;

	cl_object plus_half;
	cl_object minus_half;
	cl_object imag_unit;
	cl_object minus_imag_unit;
	cl_object imag_two;
	cl_object singlefloat_zero;
	cl_object doublefloat_zero;
	cl_object singlefloat_minus_zero;
	cl_object doublefloat_minus_zero;
#ifdef ECL_LONG_FLOAT
	cl_object longfloat_zero;
	cl_object longfloat_minus_zero;
#endif

	cl_object gensym_prefix;
	cl_object gentemp_prefix;
	cl_object gentemp_counter;

	cl_object Jan1st1970UT;

	cl_object system_properties;
	cl_object setf_definitions;

#ifdef ECL_THREADS
	cl_object processes;
	cl_object processes_spinlock;
	cl_object global_lock;
        cl_object error_lock;
        cl_object global_env_lock;
#endif
	cl_object libraries;

	cl_index max_heap_size;
	cl_object bytes_consed;
	cl_object gc_counter;
	bool gc_stats;
	int path_max;
#ifdef GBC_BOEHM
        char *safety_region;
#endif
        void *default_sigmask;
        cl_index default_sigmask_bytes;

#ifdef ECL_THREADS
        cl_index last_var_index;
        cl_object reused_indices;
#endif
	cl_object slash;

	cl_object compiler_dispatch;

        cl_object rehash_size;
        cl_object rehash_threshold;

        cl_object external_processes;
        cl_object external_processes_lock;

	cl_object known_signals;
};

extern ECL_API struct cl_core_struct cl_core;

/* alloc.c / alloc_2.c */

extern ECL_API cl_object ecl_alloc_object(cl_type t);
extern ECL_API cl_object ecl_alloc_instance(cl_index slots);
extern ECL_API cl_object ecl_cons(cl_object a, cl_object d);
extern ECL_API cl_object ecl_list1(cl_object a);
#ifdef GBC_BOEHM
extern ECL_API cl_object si_gc(cl_narg narg, ...);
extern ECL_API cl_object si_gc_dump(void);
extern ECL_API cl_object si_gc_stats(cl_object enable);
extern ECL_API void *ecl_alloc_unprotected(cl_index n);
extern ECL_API void *ecl_alloc_atomic_unprotected(cl_index n);
extern ECL_API void *ecl_alloc(cl_index n);
extern ECL_API void *ecl_alloc_atomic(cl_index n);
extern ECL_API void *ecl_alloc_uncollectable(size_t size);
extern ECL_API void ecl_free_uncollectable(void *);
extern ECL_API void ecl_dealloc(void *);
#define ecl_alloc_align(s,d) ecl_alloc(s)
#define ecl_alloc_atomic_align(s,d) ecl_alloc_atomic(s)
#define ecl_register_static_root(x) ecl_register_root(x)
extern ECL_API cl_object ecl_alloc_compact_object(cl_type t, cl_index extra_space);

extern ECL_API cl_object si_make_weak_pointer(cl_object o);
extern ECL_API cl_object si_weak_pointer_value(cl_object o);
#else
extern ECL_API cl_object si_allocate _ECL_ARGS((cl_narg narg, cl_object type, cl_object qty, ...));
extern ECL_API cl_object si_maximum_allocatable_pages _ECL_ARGS((cl_narg narg, cl_object type));
extern ECL_API cl_object si_allocated_pages _ECL_ARGS((cl_narg narg, cl_object type));
extern ECL_API cl_object si_alloc_contpage _ECL_ARGS((cl_narg narg, cl_object qty, ...));
extern ECL_API cl_object si_allocated_contiguous_pages _ECL_ARGS((cl_narg narg));
extern ECL_API cl_object si_maximum_contiguous_pages _ECL_ARGS((cl_narg narg));
extern ECL_API cl_object si_allocate_contiguous_pages _ECL_ARGS((cl_narg narg, cl_object qty, ...));
extern ECL_API cl_object si_get_hole_size _ECL_ARGS((cl_narg narg));
extern ECL_API cl_object si_set_hole_size _ECL_ARGS((cl_narg narg, cl_object size));
extern ECL_API cl_object si_ignore_maximum_pages _ECL_ARGS((cl_narg narg, ...));
extern ECL_API void *ecl_alloc(cl_index n);
extern ECL_API void *ecl_alloc_align(cl_index size, cl_index align);
extern ECL_API void *ecl_alloc_uncollectable(size_t size);
extern ECL_API void ecl_free_uncollectable(void *);
extern ECL_API void ecl_dealloc(void *p);
#define ecl_alloc_atomic(x) ecl_alloc(x)
#define ecl_alloc_atomic_align(x,s) ecl_alloc_align(x,s)
#define ecl_register_static_root(x) ecl_register_root(x);
#endif /* GBC_BOEHM */

/* all_symbols */

extern ECL_API cl_object si_mangle_name _ECL_ARGS((cl_narg narg, cl_object symbol, ...));

typedef union {
	struct {
		const char *name;
		int type;
		void *fun;
		short narg;
		cl_object value;
	} init;
	struct ecl_symbol data;
} cl_symbol_initializer;
extern ECL_API cl_symbol_initializer cl_symbols[];
extern ECL_API cl_index cl_num_symbols_in_core;

#define ECL_SYM(name,code) ((cl_object)(cl_symbols+(code)))

/* apply.c */

extern ECL_API cl_object APPLY_fixed(cl_narg n, cl_object (*f)(), cl_object *x);
extern ECL_API cl_object APPLY(cl_narg n, cl_objectfn, cl_object *x);


/* array.c */

extern ECL_API cl_object cl_row_major_aref(cl_object x, cl_object i);
extern ECL_API cl_object si_row_major_aset(cl_object x, cl_object i, cl_object v);
extern ECL_API cl_object si_make_vector(cl_object etype, cl_object dim, cl_object adj, cl_object fillp, cl_object displ, cl_object disploff);

/* for open-coding the access while preserving the bounds and type check: */
extern ECL_API void *ecl_row_major_ptr(cl_object arr, cl_index index, cl_index bytes);

extern ECL_API cl_object cl_array_element_type(cl_object a);
extern ECL_API cl_object cl_array_rank(cl_object a);
extern ECL_API cl_object cl_array_dimension(cl_object a, cl_object index);
extern ECL_API cl_object cl_array_total_size(cl_object a);
extern ECL_API cl_object cl_adjustable_array_p(cl_object a);
extern ECL_API cl_object cl_array_displacement(cl_object a);
extern ECL_API cl_object si_array_raw_data(cl_object array);
extern ECL_API cl_object si_array_element_type_byte_size(cl_object type);
extern ECL_API cl_object cl_svref(cl_object x, cl_object index);
extern ECL_API cl_object si_svset(cl_object x, cl_object index, cl_object v);
extern ECL_API cl_object cl_array_has_fill_pointer_p(cl_object a);
extern ECL_API cl_object cl_fill_pointer(cl_object a);
extern ECL_API cl_object si_fill_pointer_set(cl_object a, cl_object fp);
extern ECL_API cl_object si_replace_array(cl_object old_obj, cl_object new_obj);
extern ECL_API cl_object cl_aref _ECL_ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object si_aset _ECL_ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object si_make_pure_array(cl_object etype, cl_object dims, cl_object adj, cl_object fillp, cl_object displ, cl_object disploff);
extern ECL_API cl_object si_copy_subarray(cl_object dest, cl_object start0, cl_object orig, cl_object start1, cl_object length);
extern ECL_API cl_object si_fill_array_with_elt(cl_object array, cl_object elt, cl_object start, cl_object end);

extern ECL_API void FEwrong_dimensions(cl_object a, cl_index rank) ecl_attr_noreturn;
extern ECL_API cl_index ecl_to_index(cl_object n);
extern ECL_API cl_index ecl_array_dimension(cl_object x, cl_index n);
extern ECL_API cl_index ecl_array_rank(cl_object x);
extern ECL_API cl_object ecl_aref_unsafe(cl_object x, cl_index index);
extern ECL_API cl_object ecl_aset_unsafe(cl_object x, cl_index index, cl_object value);
extern ECL_API cl_object ecl_aref(cl_object x, cl_index index);
extern ECL_API cl_object ecl_aref1(cl_object v, cl_index index);
extern ECL_API cl_object ecl_aset(cl_object x, cl_index index, cl_object value);
extern ECL_API cl_object ecl_aset1(cl_object v, cl_index index, cl_object val);
extern ECL_API void ecl_array_allocself(cl_object x);
extern ECL_API cl_object ecl_alloc_simple_vector(cl_index l, cl_elttype aet);
extern ECL_API cl_elttype ecl_array_elttype(cl_object x);
extern ECL_API cl_elttype ecl_symbol_to_elttype(cl_object x);
extern ECL_API cl_object ecl_elttype_to_symbol(cl_elttype aet);
extern ECL_API void ecl_copy_subarray(cl_object dest, cl_index i0, cl_object orig, cl_index i1, cl_index l);
extern ECL_API void ecl_reverse_subarray(cl_object dest, cl_index i0, cl_index i1);


/* assignment.c */

extern ECL_API cl_object cl_set(cl_object var, cl_object val);
extern ECL_API cl_object cl_makunbound(cl_object sym);
extern ECL_API cl_object cl_fmakunbound(cl_object sym);
extern ECL_API cl_object si_fset _ECL_ARGS((cl_narg narg, cl_object fun, cl_object def, ...));
extern ECL_API cl_object si_get_sysprop(cl_object sym, cl_object prop);
extern ECL_API cl_object si_put_sysprop(cl_object sym, cl_object prop, cl_object value);
extern ECL_API cl_object si_rem_sysprop(cl_object sym, cl_object prop);
extern ECL_API cl_object ecl_setf_definition(cl_object fname, cl_object createp);
extern ECL_API cl_object si_setf_definition(cl_object fname, cl_object createp);

extern ECL_API void ecl_clear_compiler_properties(cl_object sym);

/* big.c */

#define _ecl_big_register0()	ecl_process_env()->big_register[0]
#define _ecl_big_register1()	ecl_process_env()->big_register[1]
#define _ecl_big_register2()	ecl_process_env()->big_register[2]
extern ECL_API cl_object _ecl_fix_times_fix(cl_fixnum x, cl_fixnum y);
extern ECL_API cl_object _ecl_big_register_copy(cl_object x);
extern ECL_API cl_object _ecl_big_register_normalize(cl_object x);
extern ECL_API cl_object _ecl_big_times_fix(cl_object x, cl_fixnum y);
extern ECL_API cl_object _ecl_big_times_big(cl_object x, cl_object y);
extern ECL_API cl_object _ecl_big_plus_fix(cl_object x, cl_fixnum y);
extern ECL_API cl_object _ecl_big_plus_big(cl_object x, cl_object y);
extern ECL_API cl_object _ecl_fix_minus_big(cl_fixnum x, cl_object y);
extern ECL_API cl_object _ecl_big_minus_big(cl_object x, cl_object y);
extern ECL_API cl_object _ecl_fix_divided_by_big(cl_fixnum x, cl_object y);
extern ECL_API cl_object _ecl_big_divided_by_fix(cl_object x, cl_fixnum y);
extern ECL_API cl_object _ecl_big_divided_by_big(cl_object x, cl_object y);
extern ECL_API cl_object _ecl_big_gcd(cl_object x, cl_object y);
extern ECL_API cl_object _ecl_big_ceiling(cl_object x, cl_object y, cl_object *r);
extern ECL_API cl_object _ecl_big_floor(cl_object x, cl_object y, cl_object *r);
extern ECL_API cl_object _ecl_big_negate(cl_object x);
extern ECL_API void _ecl_big_register_free(cl_object x);
extern ECL_API cl_object bignum1(cl_fixnum val);


/* cfun.c */

extern ECL_API cl_object si_compiled_function_name(cl_object fun);
extern ECL_API cl_object si_compiled_function_block(cl_object fun);
extern ECL_API cl_object cl_function_lambda_expression(cl_object fun);
extern ECL_API cl_object si_compiled_function_file(cl_object fun);

extern ECL_API cl_object ecl_make_cfun(cl_objectfn_fixed c_function, cl_object name, cl_object block, int narg);
extern ECL_API cl_object ecl_make_cfun_va(cl_objectfn c_function, cl_object name, cl_object block);
extern ECL_API cl_object ecl_make_cclosure_va(cl_objectfn c_function, cl_object env, cl_object block);
extern ECL_API void ecl_def_c_function(cl_object sym, cl_objectfn_fixed c_function, int narg);
extern ECL_API void ecl_def_c_macro(cl_object sym, cl_objectfn_fixed c_function, int narg);
extern ECL_API void ecl_def_c_macro_va(cl_object sym, cl_objectfn c_function);
extern ECL_API void ecl_def_c_function_va(cl_object sym, cl_objectfn c_function);
extern ECL_API void ecl_set_function_source_file_info(cl_object fun, cl_object source, cl_object position);
extern ECL_API void ecl_cmp_defmacro(cl_object data);
extern ECL_API void ecl_cmp_defun(cl_object data);


/* character.c */

extern ECL_API cl_object cl_digit_char_p _ECL_ARGS((cl_narg narg, cl_object c, ...));
extern ECL_API cl_object cl_charE _ECL_ARGS((cl_narg narg, cl_object c, ...));
extern ECL_API cl_object cl_charNE _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_charL _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_charG _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_charLE _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_charGE _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_char_equal _ECL_ARGS((cl_narg narg, cl_object c, ...));
extern ECL_API cl_object cl_char_not_equal _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_char_lessp _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_char_greaterp _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_char_not_greaterp _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_char_not_lessp _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_digit_char _ECL_ARGS((cl_narg narg, cl_object w, ...));

extern ECL_API cl_object cl_alpha_char_p(cl_object c);
extern ECL_API cl_object cl_alphanumericp(cl_object c);
extern ECL_API cl_object cl_both_case_p(cl_object c);
extern ECL_API cl_object cl_char_code(cl_object c);
extern ECL_API cl_object cl_char_downcase(cl_object c);
extern ECL_API cl_object cl_char_int(cl_object c);
extern ECL_API cl_object cl_char_name(cl_object c);
extern ECL_API cl_object cl_char_upcase(cl_object c);
extern ECL_API cl_object cl_character(cl_object x);
extern ECL_API cl_object cl_code_char(cl_object c);
extern ECL_API cl_object cl_graphic_char_p(cl_object c);
extern ECL_API cl_object cl_lower_case_p(cl_object c);
extern ECL_API cl_object cl_name_char(cl_object s);
extern ECL_API cl_object cl_standard_char_p(cl_object c);
extern ECL_API cl_object cl_upper_case_p(cl_object c);
extern ECL_API int ecl_string_case(cl_object s);

extern ECL_API bool ecl_alpha_char_p(ecl_character c);
extern ECL_API bool ecl_alphanumericp(ecl_character c);
extern ECL_API bool ecl_both_case_p(ecl_character c);
extern ECL_API ecl_character ecl_char_downcase(ecl_character c);
extern ECL_API ecl_character ecl_char_upcase(ecl_character c);
extern ECL_API bool ecl_graphic_char_p(ecl_character c);
extern ECL_API bool ecl_lower_case_p(ecl_character c);
extern ECL_API bool ecl_standard_char_p(ecl_character c);
extern ECL_API bool ecl_base_char_p(ecl_character c);
extern ECL_API bool ecl_upper_case_p(ecl_character c);

extern ECL_API int ecl_base_string_case(cl_object s);
extern ECL_API ecl_character ecl_char_code(cl_object c);
extern ECL_API ecl_base_char ecl_base_char_code(cl_object c);
extern ECL_API int ecl_digitp(ecl_character i, int r);
extern ECL_API bool ecl_char_eq(cl_object x, cl_object y);
extern ECL_API int ecl_char_cmp(cl_object x, cl_object y);
extern ECL_API bool ecl_char_equal(cl_object x, cl_object y);
extern ECL_API int ecl_char_compare(cl_object x, cl_object y);
extern ECL_API short ecl_digit_char(cl_fixnum w, cl_fixnum r);

/* clos.c */

#ifdef CLOS
extern ECL_API cl_object cl_find_class _ECL_ARGS((cl_narg narg, cl_object name, ...));
extern ECL_API cl_object cl_class_of(cl_object x);
#endif

/* cmpaux.c */

extern ECL_API cl_object si_specialp(cl_object sym);

extern ECL_API cl_fixnum ecl_ifloor(cl_fixnum x, cl_fixnum y);
extern ECL_API cl_fixnum ecl_imod(cl_fixnum x, cl_fixnum y);
extern ECL_API char ecl_to_char(cl_object x);
extern ECL_API cl_fixnum ecl_to_fixnum(cl_object x);
extern ECL_API cl_index ecl_to_unsigned_integer(cl_object x);
extern ECL_API int ecl_aref_bv(cl_object x, cl_index index);
extern ECL_API int ecl_aset_bv(cl_object x, cl_index index, int value);
extern ECL_API void cl_throw(cl_object tag) /*ecl_attr_noreturn*/;
extern ECL_API void cl_return_from(cl_object block_id, cl_object block_name) /*ecl_attr_noreturn*/;
extern ECL_API void cl_go(cl_object tag_id, cl_object label) /*ecl_attr_noreturn*/;
extern ECL_API void cl_parse_key(ecl_va_list args, int nkey, cl_object *keys, cl_object *vars, cl_object *rest, bool allow_other_keys);
extern ECL_API cl_object cl_grab_rest_args(ecl_va_list args);

/* compiler.c */

extern ECL_API cl_object si_macrolet_function(cl_object form, cl_object env);
extern ECL_API cl_object si_process_lambda_list(cl_object lambda_list, cl_object context);
extern ECL_API cl_object si_process_lambda(cl_object lambda);
extern ECL_API cl_object si_make_lambda(cl_object name, cl_object body);
extern ECL_API cl_object si_function_block_name(cl_object name);
extern ECL_API cl_object si_valid_function_name_p(cl_object name);
extern ECL_API cl_object si_process_declarations _ECL_ARGS((cl_narg narg, cl_object body, ...));

extern ECL_API cl_object si_eval_with_env _ECL_ARGS((cl_narg narg, cl_object form, ...));

/* interpreter.c */

extern ECL_API cl_object si_interpreter_stack _ECL_ARGS((cl_narg narg));
extern ECL_API cl_object ecl_stack_frame_open(cl_env_ptr env, cl_object f, cl_index size);
extern ECL_API void ecl_stack_frame_push(cl_object f, cl_object o);
extern ECL_API void ecl_stack_frame_push_values(cl_object f);
extern ECL_API cl_object ecl_stack_frame_pop_values(cl_object f);
extern ECL_API void ecl_stack_frame_close(cl_object f);
#define si_apply_from_stack_frame ecl_apply_from_stack_frame

extern ECL_API void FEstack_underflow(void) ecl_attr_noreturn;
extern ECL_API void FEstack_advance(void) ecl_attr_noreturn;
extern ECL_API cl_object *ecl_stack_grow(cl_env_ptr env);
extern ECL_API cl_object *ecl_stack_set_size(cl_env_ptr env, cl_index new_size);
extern ECL_API cl_index ecl_stack_push_values(cl_env_ptr env);
extern ECL_API void ecl_stack_pop_values(cl_env_ptr env, cl_index n);
extern ECL_API cl_object ecl_interpret(cl_object frame, cl_object env, cl_object bytecodes);
extern ECL_API cl_object _ecl_bytecodes_dispatch(cl_narg narg, ...);
extern ECL_API cl_object _ecl_bclosure_dispatch(cl_narg narg, ...);

/* disassembler.c */

extern ECL_API cl_object si_bc_disassemble(cl_object v);
extern ECL_API cl_object si_bc_split(cl_object v);
extern ECL_API cl_object si_bc_join(cl_object lex, cl_object code, cl_object data, cl_object name);

/* error.c */

extern ECL_API cl_object cl_error _ECL_ARGS((cl_narg narg, cl_object eformat, ...)) ecl_attr_noreturn;
extern ECL_API cl_object cl_cerror _ECL_ARGS((cl_narg narg, cl_object cformat, cl_object eformat, ...));

extern ECL_API void ecl_internal_error(const char *s) ecl_attr_noreturn;
extern ECL_API void ecl_unrecoverable_error(cl_env_ptr the_env, const char *message) ecl_attr_noreturn;
extern ECL_API void ecl_cs_overflow(void) /*ecl_attr_noreturn*/;
extern ECL_API void FEprogram_error(const char *s, int narg, ...) ecl_attr_noreturn;
extern ECL_API void FEprogram_error_noreturn(const char *s, int narg, ...) ecl_attr_noreturn;
extern ECL_API void FEcontrol_error(const char *s, int narg, ...) ecl_attr_noreturn;
extern ECL_API void FEreader_error(const char *s, cl_object stream, int narg, ...) ecl_attr_noreturn;
#define FEparse_error FEreader_error
extern ECL_API void FEerror(const char *s, int narg, ...) ecl_attr_noreturn;
extern ECL_API void FEcannot_open(cl_object fn) ecl_attr_noreturn;
extern ECL_API void FEend_of_file(cl_object strm) ecl_attr_noreturn;
extern ECL_API void FEclosed_stream(cl_object strm) ecl_attr_noreturn;
extern ECL_API void FEwrong_type_argument(cl_object type, cl_object value) ecl_attr_noreturn;
extern ECL_API void FEwrong_type_only_arg(cl_object function, cl_object type, cl_object value) ecl_attr_noreturn;
extern ECL_API void FEwrong_type_nth_arg(cl_object function, cl_narg narg, cl_object value, cl_object type) ecl_attr_noreturn;
extern ECL_API void FEwrong_type_key_arg(cl_object function, cl_object keyo, cl_object type, cl_object value) ecl_attr_noreturn;
extern ECL_API void FEwrong_num_arguments(cl_object fun) ecl_attr_noreturn;
extern ECL_API void FEwrong_num_arguments_anonym(void) ecl_attr_noreturn;
extern ECL_API void FEwrong_index(cl_object function, cl_object a, int which, cl_object ndx, cl_index nonincl_limit) ecl_attr_noreturn;
extern ECL_API void FEunbound_variable(cl_object sym) ecl_attr_noreturn;
extern ECL_API void FEinvalid_macro_call(cl_object obj) ecl_attr_noreturn;
extern ECL_API void FEinvalid_variable(const char *s, cl_object obj) ecl_attr_noreturn;
extern ECL_API void FEassignment_to_constant(cl_object v) ecl_attr_noreturn;
extern ECL_API void FEundefined_function(cl_object fname) ecl_attr_noreturn;
extern ECL_API void FEinvalid_function(cl_object obj) ecl_attr_noreturn;
extern ECL_API void FEinvalid_function_name(cl_object obj) ecl_attr_noreturn;
extern ECL_API void FEprint_not_readable(cl_object obj) ecl_attr_noreturn;
extern ECL_API cl_object CEerror(cl_object c, const char *err_str, int narg, ...);
extern ECL_API void FElibc_error(const char *msg, int narg, ...) ecl_attr_noreturn;
#if defined(ECL_MS_WINDOWS_HOST) || defined(cygwin)
extern ECL_API void FEwin32_error(const char *msg, int narg, ...) ecl_attr_noreturn;
#endif
extern ECL_API cl_object si_signal_type_error(cl_object value, cl_object type) ecl_attr_noreturn;


/* eval.c */

extern ECL_API cl_object cl_funcall _ECL_ARGS((cl_narg narg, cl_object fun, ...));
extern ECL_API cl_object cl_apply _ECL_ARGS((cl_narg narg, cl_object fun, cl_object arg, ...));
extern ECL_API cl_object si_safe_eval _ECL_ARGS((cl_narg narg, cl_object form, cl_object env, ...));
#define cl_safe_eval(form,env,value) si_safe_eval(3,form,env,value)
extern ECL_API cl_object *_ecl_va_sp(cl_narg narg);
extern ECL_API cl_object si_unlink_symbol(cl_object s);
extern ECL_API cl_object cl_eval(cl_object form);
extern ECL_API cl_object cl_constantp _ECL_ARGS((cl_narg narg, cl_object arg, ...));

#define funcall cl_funcall
extern ECL_API cl_object cl_apply_from_stack(cl_index narg, cl_object fun);
extern ECL_API cl_object ecl_apply_from_stack_frame(cl_object f, cl_object o);
extern ECL_API cl_objectfn ecl_function_dispatch(cl_env_ptr env, cl_object f);
extern ECL_API cl_object _ecl_link_call(cl_object sym, cl_objectfn *pLK, cl_object cblock, int narg, ecl_va_list args);

/* ffi/cdata.d */
extern ECL_API cl_object si_get_cdata(cl_object filename);
extern ECL_API cl_object si_add_cdata(cl_object filename, cl_object cdata);

/* ffi/libraries.d */
extern ECL_API cl_object ecl_make_codeblock();
extern ECL_API cl_object ecl_library_open(cl_object filename, bool force_reload);
extern ECL_API void *ecl_library_symbol(cl_object block, const char *symbol, bool lock);
extern ECL_API cl_object ecl_library_error(cl_object block);
extern ECL_API void ecl_library_close(cl_object block);
extern ECL_API void ecl_library_close_all(void);

/* ffi/mmap.d */
extern ECL_API cl_object si_mmap _ECL_ARGS((cl_narg narg, cl_object filename, ...));
extern ECL_API cl_object si_munmap(cl_object map);
extern ECL_API cl_object si_mmap_array(cl_object map);

/* ffi/backtrace.d */
extern ECL_API cl_object si_dump_c_backtrace(cl_object size);
extern ECL_API cl_object si_backtrace(cl_object start, cl_object end);

/* ffi.c */

extern ECL_API cl_object si_allocate_foreign_data(cl_object tag, cl_object size);
extern ECL_API cl_object si_foreign_elt_type_p(cl_object f);
extern ECL_API cl_object si_foreign_data_p(cl_object f);
extern ECL_API cl_object si_foreign_data_address(cl_object f);
extern ECL_API cl_object si_foreign_data_equal(cl_object f1, cl_object f2);
extern ECL_API cl_object si_foreign_data_pointer(cl_object f, cl_object ndx, cl_object size, cl_object tag);
extern ECL_API cl_object si_foreign_data_ref(cl_object f, cl_object ndx, cl_object size, cl_object tag);
extern ECL_API cl_object si_foreign_data_ref_elt(cl_object f, cl_object ndx, cl_object tag);
extern ECL_API cl_object si_foreign_data_set(cl_object f, cl_object ndx, cl_object value);
extern ECL_API cl_object si_foreign_data_set_elt(cl_object f, cl_object ndx, cl_object tag, cl_object value);
extern ECL_API cl_object si_foreign_data_tag(cl_object x);
extern ECL_API cl_object si_foreign_data_recast(cl_object f, cl_object size, cl_object tag);
extern ECL_API cl_object si_free_foreign_data(cl_object x);
extern ECL_API cl_object si_make_foreign_data_from_array(cl_object x);
extern ECL_API cl_object si_null_pointer_p(cl_object f);
extern ECL_API cl_object si_size_of_foreign_elt_type(cl_object tag);
extern ECL_API cl_object si_alignment_of_foreign_elt_type(cl_object tag);
extern ECL_API cl_object si_load_foreign_module(cl_object module);
extern ECL_API cl_object si_find_foreign_symbol(cl_object var, cl_object module, cl_object type, cl_object size);
extern ECL_API cl_object si_call_cfun(cl_narg, cl_object fun, cl_object return_type, cl_object arg_types, cl_object args, ...);
extern ECL_API cl_object si_make_dynamic_callback(cl_narg, cl_object fun, cl_object sym, cl_object return_type, cl_object arg_types, ...);
extern ECL_API cl_object si_free_ffi_closure(cl_object closure);

/* Only foreign data types can be coerced to a pointer */
#define ecl_make_pointer(x) ecl_make_foreign_data(ECL_NIL,0,(x))
#define ecl_to_pointer(x) ecl_foreign_data_pointer_safe(x)
extern ECL_API cl_object ecl_make_foreign_data(cl_object tag, cl_index size, void *data);
extern ECL_API cl_object ecl_allocate_foreign_data(cl_object tag, cl_index size);
extern ECL_API void *ecl_foreign_data_pointer_safe(cl_object f);
extern ECL_API char *ecl_base_string_pointer_safe(cl_object f);
extern ECL_API cl_object ecl_null_terminated_base_string(cl_object s);
extern ECL_API cl_object ecl_foreign_data_ref_elt(void *p, enum ecl_ffi_tag type);
extern ECL_API void ecl_foreign_data_set_elt(void *p, enum ecl_ffi_tag type, cl_object value);

/* file.c */

#define ECL_LISTEN_NO_CHAR	0
#define ECL_LISTEN_AVAILABLE	1
#define ECL_LISTEN_EOF		-1

extern ECL_API cl_object cl_make_synonym_stream(cl_object sym);
extern ECL_API cl_object cl_synonym_stream_symbol(cl_object strm);
extern ECL_API cl_object cl_make_two_way_stream(cl_object strm1, cl_object strm2);
extern ECL_API cl_object cl_two_way_stream_input_stream(cl_object strm);
extern ECL_API cl_object cl_two_way_stream_output_stream(cl_object strm);
extern ECL_API cl_object cl_make_echo_stream(cl_object strm1, cl_object strm2);
extern ECL_API cl_object cl_echo_stream_input_stream(cl_object strm);
extern ECL_API cl_object cl_echo_stream_output_stream(cl_object strm);
extern ECL_API cl_object cl_make_string_output_stream _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_get_output_stream_string(cl_object strm);
extern ECL_API cl_object cl_streamp(cl_object strm);
extern ECL_API cl_object cl_input_stream_p(cl_object strm);
extern ECL_API cl_object cl_output_stream_p(cl_object strm);
extern ECL_API cl_object cl_stream_element_type(cl_object strm);
extern ECL_API cl_object cl_stream_external_format(cl_object strm);
extern ECL_API cl_object cl_file_length(cl_object strm);
extern ECL_API cl_object si_get_string_input_stream_index(cl_object strm);
extern ECL_API cl_object si_make_string_output_stream_from_string(cl_object strng);
extern ECL_API cl_object si_copy_stream(cl_object in, cl_object out);
extern ECL_API cl_object cl_open_stream_p(cl_object strm);
extern ECL_API cl_object cl_make_broadcast_stream _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_broadcast_stream_streams(cl_object strm);
extern ECL_API cl_object cl_make_concatenated_stream _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_concatenated_stream_streams(cl_object strm);
extern ECL_API cl_object cl_make_string_input_stream _ECL_ARGS((cl_narg narg, cl_object strng, ...));
extern ECL_API cl_object si_make_sequence_input_stream _ECL_ARGS((cl_narg narg, cl_object vector, ...));
extern ECL_API cl_object si_make_sequence_output_stream _ECL_ARGS((cl_narg narg, cl_object vector, ...));
extern ECL_API cl_object cl_close _ECL_ARGS((cl_narg narg, cl_object strm, ...));
extern ECL_API cl_object cl_open _ECL_ARGS((cl_narg narg, cl_object filename, ...));
extern ECL_API cl_object cl_file_position _ECL_ARGS((cl_narg narg, cl_object file_stream, ...));
extern ECL_API cl_object cl_file_string_length(cl_object stream, cl_object string);
extern ECL_API cl_object si_do_write_sequence(cl_object string, cl_object stream, cl_object start, cl_object end);
extern ECL_API cl_object si_do_read_sequence(cl_object string, cl_object stream, cl_object start, cl_object end);
extern ECL_API cl_object si_file_column(cl_object strm);
extern ECL_API cl_object cl_interactive_stream_p(cl_object strm);
extern ECL_API cl_object si_set_buffering_mode(cl_object strm, cl_object mode);
extern ECL_API cl_object si_stream_external_format_set(cl_object strm, cl_object format);

extern ECL_API bool ecl_input_stream_p(cl_object strm);
extern ECL_API bool ecl_output_stream_p(cl_object strm);
extern ECL_API cl_object ecl_stream_element_type(cl_object strm);
extern ECL_API bool ecl_interactive_stream_p(cl_object strm);
extern ECL_API cl_object ecl_open_stream(cl_object fn, enum ecl_smmode smm, cl_object if_exists, cl_object if_does_not_exist, cl_fixnum byte_size, int flags, cl_object external_format);
extern ECL_API cl_object ecl_make_string_input_stream(cl_object strng, cl_index istart, cl_index iend);
extern ECL_API cl_object ecl_make_string_output_stream(cl_index line_length, int extended);
extern ECL_API cl_object ecl_read_byte(cl_object strm);
extern ECL_API void ecl_write_byte(cl_object byte, cl_object strm);
extern ECL_API ecl_character ecl_read_char_noeof(cl_object strm);
extern ECL_API ecl_character ecl_read_char(cl_object strm);
extern ECL_API void ecl_unread_char(ecl_character c, cl_object strm);
extern ECL_API ecl_character ecl_peek_char(cl_object strm);
extern ECL_API ecl_character ecl_write_char(ecl_character c, cl_object strm);
extern ECL_API void writestr_stream(const char *s, cl_object strm);
extern ECL_API void ecl_force_output(cl_object strm);
extern ECL_API void ecl_finish_output(cl_object strm);
extern ECL_API void ecl_clear_input(cl_object strm);
extern ECL_API void ecl_clear_output(cl_object strm);
extern ECL_API bool ecl_listen_stream(cl_object strm);
extern ECL_API cl_object ecl_file_position(cl_object strm);
extern ECL_API cl_object ecl_file_position_set(cl_object strm, cl_object disp);
extern ECL_API cl_object ecl_file_length(cl_object strm);
extern ECL_API int ecl_file_column(cl_object strm);
extern ECL_API cl_fixnum ecl_normalize_stream_element_type(cl_object element);
extern ECL_API cl_object ecl_make_stream_from_fd(cl_object fname, int fd, enum ecl_smmode smm, cl_fixnum byte_size, int flags, cl_object external_format);
extern ECL_API cl_object ecl_make_stream_from_FILE(cl_object fname, void *fd, enum ecl_smmode smm, cl_fixnum byte_size, int flags, cl_object external_format);
extern ECL_API cl_object si_file_stream_fd(cl_object s);
extern ECL_API int ecl_stream_to_handle(cl_object s, bool output);

/* finalize.c */

extern ECL_API void ecl_set_finalizer_unprotected(cl_object o, cl_object finalizer);
extern ECL_API cl_object si_get_finalizer(cl_object o);
extern ECL_API cl_object si_set_finalizer(cl_object o, cl_object finalizer);

/* format.c */

extern ECL_API cl_object cl_format _ECL_ARGS((cl_narg narg, cl_object stream, cl_object string, ...));

/* gbc.c */

#if !defined(GBC_BOEHM)
extern ECL_API cl_object si_room_report _ECL_ARGS((cl_narg narg));
extern ECL_API cl_object si_reset_gc_count _ECL_ARGS((cl_narg narg));
extern ECL_API cl_object si_gc_time _ECL_ARGS((cl_narg narg));
extern ECL_API cl_object si_gc(cl_object area);
#define GC_enabled() GC_enable
#define GC_enable() GC_enable = TRUE;
#define GC_disable() GC_enable = FALSE;
extern ECL_API bool GC_enable;
extern ECL_API cl_object (*GC_enter_hook)(void);
extern ECL_API cl_object (*GC_exit_hook)(void);
extern ECL_API void ecl_register_root(cl_object *p);
extern ECL_API void ecl_gc(cl_type t);
#endif

#ifdef GBC_BOEHM
#define GC_enabled() (!GC_dont_gc)
#define GC_enable() GC_dont_gc = FALSE;
#define GC_disable() GC_dont_gc = TRUE;
extern ECL_API void ecl_register_root(cl_object *p);
#endif /* GBC_BOEHM */


/* gfun.c */

#ifdef CLOS
extern ECL_API void _ecl_set_method_hash_size(cl_env_ptr env, cl_index size);
extern ECL_API cl_object si_clear_gfun_hash(cl_object what);
extern ECL_API cl_object clos_set_funcallable_instance_function(cl_object x, cl_object function_or_t);
extern ECL_API cl_object si_generic_function_p(cl_object instance);

extern ECL_API cl_object _ecl_standard_dispatch(cl_object frame, cl_object fun);
#endif /* CLOS */


/* hash.c */

extern ECL_API cl_object cl__make_hash_table(cl_object test, cl_object size, cl_object rehash_size, cl_object rehash_threshold);
extern ECL_API cl_object cl_hash_table_p(cl_object ht);
extern ECL_API cl_object si_hash_set(cl_object key, cl_object ht, cl_object val);
extern ECL_API cl_object cl_remhash(cl_object key, cl_object ht);
extern ECL_API cl_object cl_clrhash(cl_object ht);
extern ECL_API cl_object cl_hash_table_count(cl_object ht);
extern ECL_API cl_object cl_sxhash(cl_object key);
extern ECL_API cl_object cl_maphash(cl_object fun, cl_object ht);
extern ECL_API cl_object cl_hash_table_rehash_size(cl_object ht);
extern ECL_API cl_object cl_hash_table_rehash_threshold(cl_object ht);
extern ECL_API cl_object cl_hash_table_size(cl_object ht);
extern ECL_API cl_object cl_hash_table_test(cl_object ht);
extern ECL_API cl_object si_hash_table_iterator(cl_object ht);
extern ECL_API cl_object cl_make_hash_table _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_gethash _ECL_ARGS((cl_narg narg, cl_object key, cl_object ht, ...));
extern ECL_API cl_object si_copy_hash_table(cl_object orig);
extern ECL_API cl_object si_hash_eql _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_hash_equal _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_hash_equalp _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_hash_table_content(cl_object ht);
extern ECL_API cl_object si_hash_table_fill(cl_object ht, cl_object values);
extern ECL_API cl_object si_hash_table_weakness(cl_object ht);

extern ECL_API cl_object ecl_sethash(cl_object key, cl_object hashtable, cl_object value);
extern ECL_API cl_object ecl_gethash(cl_object key, cl_object hash);
extern ECL_API cl_object ecl_gethash_safe(cl_object key, cl_object hash, cl_object def);
extern ECL_API bool ecl_remhash(cl_object key, cl_object hash);
extern ECL_API cl_object _ecl_sethash(cl_object key, cl_object hashtable, cl_object value);
extern ECL_API cl_index ecl_hash_table_count(cl_object hash);

/* instance.c */

#ifdef CLOS
extern ECL_API cl_object si_allocate_raw_instance(cl_object orig, cl_object clas, cl_object size);
extern ECL_API cl_object si_instance_class(cl_object x);
extern ECL_API cl_object si_instance_class_set(cl_object x, cl_object y);
extern ECL_API cl_object si_instance_ref(cl_object x, cl_object index);
extern ECL_API cl_object clos_safe_instance_ref(cl_object x, cl_object index);
extern ECL_API cl_object si_instance_set(cl_object x, cl_object index, cl_object value);
extern ECL_API cl_object si_instancep(cl_object x);
extern ECL_API cl_object si_unbound(void);
extern ECL_API cl_object si_sl_boundp(cl_object x);
extern ECL_API cl_object si_sl_makunbound(cl_object x, cl_object index);
extern ECL_API cl_object si_instance_sig(cl_object x);
extern ECL_API cl_object si_instance_sig_set(cl_object x);

extern ECL_API cl_object ecl_allocate_instance(cl_object clas, cl_index size);
extern ECL_API cl_object ecl_instance_ref(cl_object x, cl_fixnum i);
extern ECL_API cl_object ecl_instance_set(cl_object x, cl_fixnum i, cl_object v);
extern ECL_API cl_object si_copy_instance(cl_object x);

extern ECL_API cl_object ecl_slot_value(cl_object x, const char *slot);
extern ECL_API cl_object ecl_slot_value_set(cl_object x, const char *slot, cl_object y);
#endif /* CLOS */


/* list.c */

#define cl_rest cl_cdr
#define cl_first cl_car
#define cl_second cl_cadr
#define cl_third cl_caddr
#define cl_fourth cl_cadddr

extern ECL_API cl_object cl_fifth(cl_object x);
extern ECL_API cl_object cl_sixth(cl_object x);
extern ECL_API cl_object cl_seventh(cl_object x);
extern ECL_API cl_object cl_eighth(cl_object x);
extern ECL_API cl_object cl_ninth(cl_object x);
extern ECL_API cl_object cl_tenth(cl_object x);
extern ECL_API cl_object cl_endp(cl_object x);
extern ECL_API cl_object cl_list_length(cl_object x);
extern ECL_API cl_object cl_nth(cl_object n, cl_object x);
extern ECL_API cl_object cl_nthcdr(cl_object n, cl_object x);
extern ECL_API cl_object cl_copy_list(cl_object x);
extern ECL_API cl_object cl_copy_alist(cl_object x);
extern ECL_API cl_object cl_copy_tree(cl_object x);
extern ECL_API cl_object cl_revappend(cl_object x, cl_object y);
extern ECL_API cl_object cl_ldiff(cl_object x, cl_object y);
extern ECL_API cl_object cl_rplaca(cl_object x, cl_object v);
extern ECL_API cl_object cl_rplacd(cl_object x, cl_object v);
extern ECL_API cl_object cl_tailp(cl_object y, cl_object x);
extern ECL_API cl_object si_memq(cl_object x, cl_object l);
extern ECL_API cl_object cl_nreconc(cl_object x, cl_object y);
extern ECL_API cl_object cl_cons(cl_object x, cl_object y);
extern ECL_API cl_object cl_acons(cl_object x, cl_object y, cl_object z);
extern ECL_API cl_object cl_list _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_listX _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_append _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_tree_equal _ECL_ARGS((cl_narg narg, cl_object x, cl_object y, ...));
extern ECL_API cl_object cl_last _ECL_ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_make_list _ECL_ARGS((cl_narg narg, cl_object size, ...));
extern ECL_API cl_object cl_nconc _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_butlast _ECL_ARGS((cl_narg narg, cl_object lis, ...));
extern ECL_API cl_object cl_nbutlast _ECL_ARGS((cl_narg narg, cl_object lis, ...));
extern ECL_API cl_object cl_subst _ECL_ARGS((cl_narg narg, cl_object new_obj, cl_object old_obj, cl_object tree, ...));
extern ECL_API cl_object cl_nsubst _ECL_ARGS((cl_narg narg, cl_object new_obj, cl_object old_obj, cl_object tree, ...));
extern ECL_API cl_object cl_sublis _ECL_ARGS((cl_narg narg, cl_object alist, cl_object tree, ...));
extern ECL_API cl_object cl_nsublis _ECL_ARGS((cl_narg narg, cl_object alist, cl_object tree, ...));
extern ECL_API cl_object cl_member _ECL_ARGS((cl_narg narg, cl_object item, cl_object list, ...));
extern ECL_API cl_object si_member1 (cl_object item, cl_object list, cl_object test, cl_object test_not, cl_object key);
extern ECL_API cl_object cl_adjoin _ECL_ARGS((cl_narg narg, cl_object item, cl_object list, ...));
extern ECL_API cl_object cl_pairlis _ECL_ARGS((cl_narg narg, cl_object keys, cl_object data, ...));
extern ECL_API cl_object cl_rassoc _ECL_ARGS((cl_narg narg, cl_object item, cl_object alist, ...));
extern ECL_API cl_object cl_assoc _ECL_ARGS((cl_narg narg, cl_object item, cl_object alist, ...));
extern ECL_API cl_object si_proper_list_p(cl_object V1);

extern ECL_API cl_object ecl_last(cl_object x, cl_index n);
extern ECL_API cl_object ecl_butlast(cl_object x, cl_index n);
extern ECL_API cl_object ecl_nbutlast(cl_object x, cl_index n);
extern ECL_API cl_object ecl_append(cl_object x, cl_object y);
extern ECL_API bool ecl_endp(cl_object x);
extern ECL_API cl_object ecl_nth(cl_fixnum n, cl_object x);
extern ECL_API cl_object ecl_nthcdr(cl_fixnum n, cl_object x);
extern ECL_API cl_object ecl_nconc(cl_object x, cl_object y);
extern ECL_API bool ecl_member_eq(cl_object x, cl_object l);
extern ECL_API cl_object ecl_memql(cl_object x, cl_object l);
extern ECL_API cl_object ecl_member(cl_object x, cl_object l);
extern ECL_API cl_object ecl_assq(cl_object x, cl_object l);
extern ECL_API cl_object ecl_assql(cl_object x, cl_object l);
extern ECL_API cl_object ecl_assoc(cl_object x, cl_object l);
extern ECL_API cl_object ecl_assqlp(cl_object x, cl_object l);
extern ECL_API cl_object ecl_remove_eq(cl_object x, cl_object l);
extern ECL_API cl_object ecl_delete_eq(cl_object x, cl_object l);
#define si_cons_car cl_car
#define si_cons_cdr cl_cdr


/* load.c */

extern ECL_API cl_object si_load_bytecodes(cl_object file, cl_object verbose, cl_object print, cl_object format);
extern ECL_API cl_object si_load_source(cl_object file, cl_object verbose, cl_object print, cl_object format);
extern ECL_API cl_object si_load_binary(cl_object file, cl_object verbose, cl_object print, cl_object format);
extern ECL_API cl_object cl_load _ECL_ARGS((cl_narg narg, cl_object pathname, ...));

/* macros.c */

extern ECL_API cl_object cl_macroexpand _ECL_ARGS((cl_narg narg, cl_object form, ...));
extern ECL_API cl_object cl_macroexpand_1 _ECL_ARGS((cl_narg narg, cl_object form, ...));
extern ECL_API cl_object cl_macro_function _ECL_ARGS((cl_narg narg, cl_object sym, ...));


/* main.c */

extern ECL_API cl_object si_argc(void);
extern ECL_API cl_object si_argv(cl_object index);
extern ECL_API cl_object si_getenv(cl_object var);
extern ECL_API cl_object si_setenv(cl_object var, cl_object value);
extern ECL_API cl_object si_environ(void);
extern ECL_API cl_object si_pointer(cl_object x);
extern ECL_API cl_object si_quit _ECL_ARGS((cl_narg narg, ...)) /*ecl_attr_noreturn*/;
extern ECL_API cl_object si_exit _ECL_ARGS((cl_narg narg, ...)) ecl_attr_noreturn;

typedef enum {
	ECL_OPT_INCREMENTAL_GC = 0,
	ECL_OPT_TRAP_SIGSEGV,
	ECL_OPT_TRAP_SIGFPE,
	ECL_OPT_TRAP_SIGINT,
	ECL_OPT_TRAP_SIGILL,
	ECL_OPT_TRAP_SIGBUS,
        ECL_OPT_TRAP_SIGPIPE,
        ECL_OPT_TRAP_SIGCHLD,
	ECL_OPT_TRAP_INTERRUPT_SIGNAL,
	ECL_OPT_SIGNAL_HANDLING_THREAD,
	ECL_OPT_SIGNAL_QUEUE_SIZE,
	ECL_OPT_BOOTED,
	ECL_OPT_BIND_STACK_SIZE,
	ECL_OPT_BIND_STACK_SAFETY_AREA,
	ECL_OPT_FRAME_STACK_SIZE,
	ECL_OPT_FRAME_STACK_SAFETY_AREA,
	ECL_OPT_LISP_STACK_SIZE,
	ECL_OPT_LISP_STACK_SAFETY_AREA,
	ECL_OPT_C_STACK_SIZE,
	ECL_OPT_C_STACK_SAFETY_AREA,
	ECL_OPT_SIGALTSTACK_SIZE,
	ECL_OPT_HEAP_SIZE,
	ECL_OPT_HEAP_SAFETY_AREA,
        ECL_OPT_THREAD_INTERRUPT_SIGNAL,
        ECL_OPT_SET_GMP_MEMORY_FUNCTIONS,
	ECL_OPT_USE_SETMODE_ON_FILES,
	ECL_OPT_LIMIT
} ecl_option;

extern ECL_API const char *ecl_self;
extern ECL_API void ecl_set_option(int option, cl_fixnum value);
extern ECL_API cl_fixnum ecl_get_option(int option);
extern ECL_API int cl_boot(int argc, char **argv);
extern ECL_API void cl_shutdown(void);
#if defined(ECL_MS_WINDOWS_HOST)
extern ECL_API void ecl_get_commandline_args(int* argc, char*** argv);
#endif


/* mapfun.c */

extern ECL_API cl_object cl_mapcar _ECL_ARGS((cl_narg narg, cl_object fun, ...));
extern ECL_API cl_object cl_maplist _ECL_ARGS((cl_narg narg, cl_object fun, ...));
extern ECL_API cl_object cl_mapc _ECL_ARGS((cl_narg narg, cl_object fun, ...));
extern ECL_API cl_object cl_mapl _ECL_ARGS((cl_narg narg, cl_object fun, ...));
extern ECL_API cl_object cl_mapcan _ECL_ARGS((cl_narg narg, cl_object fun, ...));
extern ECL_API cl_object cl_mapcon _ECL_ARGS((cl_narg narg, cl_object fun, ...));


/* multival.c */

extern ECL_API cl_object cl_values_list(cl_object list);
extern ECL_API cl_object cl_values _ECL_ARGS((cl_narg narg, ...));


/* num_arith.c */

extern ECL_API cl_object cl_conjugate(cl_object c);
extern ECL_API cl_object cl_1P(cl_object x);
extern ECL_API cl_object cl_1M(cl_object x);
extern ECL_API cl_object cl_X _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_P _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_M _ECL_ARGS((cl_narg narg, cl_object num, ...));
extern ECL_API cl_object cl_N _ECL_ARGS((cl_narg narg, cl_object num, ...));
extern ECL_API cl_object cl_gcd _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_lcm _ECL_ARGS((cl_narg narg, ...));

extern ECL_API cl_object ecl_times(cl_object x, cl_object y);
extern ECL_API cl_object number_to_complex(cl_object x);
extern ECL_API cl_object ecl_plus(cl_object x, cl_object y);
extern ECL_API cl_object ecl_minus(cl_object x, cl_object y);
extern ECL_API cl_object ecl_negate(cl_object x);
extern ECL_API cl_object ecl_divide(cl_object x, cl_object y);
extern ECL_API cl_object ecl_integer_divide(cl_object x, cl_object y);
extern ECL_API cl_object ecl_gcd(cl_object x, cl_object y);
extern ECL_API cl_object ecl_one_plus(cl_object x);
extern ECL_API cl_object ecl_one_minus(cl_object x);
extern ECL_API cl_object ecl_conjugate(cl_object x);


/* number.c */

extern ECL_API cl_fixnum fixint(cl_object x);
extern ECL_API cl_index  fixnnint(cl_object x);
extern ECL_API cl_object ecl_make_integer(cl_fixnum i);
extern ECL_API cl_object ecl_make_unsigned_integer(cl_index i);
extern ECL_API int ecl_to_bit(cl_object o);
extern ECL_API ecl_uint8_t ecl_to_uint8_t(cl_object o);
extern ECL_API ecl_int8_t ecl_to_int8_t(cl_object o);
#define ecl_make_uint8_t(i) ecl_make_fixnum(i)
#define ecl_make_int8_t(i) ecl_make_fixnum(i)
#if FIXNUM_BITS < 32
# error "Unsupported platforms with FIXNUM_BITS < 32"
#endif
#ifdef ecl_uint16_t
extern ECL_API ecl_uint16_t ecl_to_uint16_t(cl_object o);
extern ECL_API ecl_int16_t ecl_to_int16_t(cl_object o);
# define ecl_make_uint16_t(i) ecl_make_fixnum(i)
# define ecl_make_int16_t(i) ecl_make_fixnum(i)
#endif /* ecl_uint16_t */
extern ECL_API unsigned short ecl_to_ushort(cl_object o);
extern ECL_API short ecl_to_short(cl_object o);
#define ecl_make_short(n) ecl_make_fixnum(n)
#define ecl_make_ushort(n) ecl_make_fixnum(n)
#ifdef ecl_uint32_t
# if FIXNUM_BITS == 32
#  define ecl_to_uint32_t fixnnint
#  define ecl_to_int32_t fixint
#  define ecl_make_uint32_t ecl_make_unsigned_integer
#  define ecl_make_int32_t ecl_make_integer
# else
#  define ecl_make_uint32_t(i) ecl_make_fixnum(i)
#  define ecl_make_int32_t(i) ecl_make_fixnum(i)
extern ECL_API ecl_uint32_t ecl_to_uint32_t(cl_object o);
extern ECL_API ecl_int32_t ecl_to_int32_t(cl_object o);
# endif
#endif /* ecl_uint32_t */
#ifdef ecl_uint64_t
# if FIXNUM_BITS >= 64
#  define ecl_to_uint64_t fixnnint
#  define ecl_to_int64_t fixint
#  define ecl_make_uint64_t ecl_make_unsigned_integer
#  define ecl_make_int64_t ecl_make_integer
# else
extern ECL_API ecl_uint64_t ecl_to_uint64_t(cl_object p);
extern ECL_API ecl_int64_t ecl_to_int64_t(cl_object p);
extern ECL_API cl_object ecl_make_uint64_t(ecl_uint64_t i);
extern ECL_API cl_object ecl_make_int64_t(ecl_int64_t i);
# endif
#endif /* ecl_uint64_t */
#if ECL_INT_BITS == 32
# define ecl_to_uint ecl_to_uint32_t
# define ecl_to_int ecl_to_int32_t
# define ecl_make_uint ecl_make_uint32_t
# define ecl_make_int ecl_make_int32_t
#else
#  if ECL_INT_BITS == 64
#   define ecl_to_uint ecl_to_uint64_t
#   define ecl_to_int ecl_to_int64_t
#   define ecl_make_uint ecl_make_uint64_t
#   define ecl_make_int ecl_make_int64_t
#  else
#   error "Currently ECL expects 'int' type to have 32 or 64 bits"
#  endif
#endif
#if ECL_LONG_BITS == 32
# define ecl_to_ulong ecl_to_uint32_t
# define ecl_to_long ecl_to_int32_t
# define ecl_make_ulong ecl_make_uint32_t
# define ecl_make_long ecl_make_int32_t
#else
# if ECL_LONG_BITS == 64
#  define ecl_to_ulong ecl_to_uint64_t
#  define ecl_to_long ecl_to_int64_t
#  define ecl_make_ulong ecl_make_uint64_t
#  define ecl_make_long ecl_make_int64_t
# else
#   error "Currently ECL expects 'long' type to have 32 or 64 bits"
# endif
#endif
#ifdef ecl_long_long_t
extern ECL_API ecl_ulong_long_t ecl_to_ulong_long(cl_object p);
extern ECL_API ecl_long_long_t ecl_to_long_long(cl_object p);
extern ECL_API cl_object ecl_make_ulong_long(ecl_ulong_long_t i);
extern ECL_API cl_object ecl_make_long_long(ecl_long_long_t i);
#endif /* ecl_long_long_t */

extern ECL_API cl_object ecl_make_ratio(cl_object num, cl_object den);
extern ECL_API cl_object ecl_make_single_float(float f);
extern ECL_API cl_object ecl_make_double_float(double f);
extern ECL_API cl_object ecl_make_complex(cl_object r, cl_object i);
extern ECL_API cl_object cl_rational(cl_object x);
#define cl_rationalize cl_rational
extern ECL_API float ecl_to_float(cl_object x);
extern ECL_API double ecl_to_double(cl_object x);
#ifdef ECL_LONG_FLOAT
extern ECL_API long double ecl_to_long_double(cl_object x);
extern ECL_API cl_object ecl_make_long_float(long double f);
#endif

/* num_co.c */

extern ECL_API cl_object cl_numerator(cl_object x);
extern ECL_API cl_object cl_denominator(cl_object x);
extern ECL_API cl_object cl_mod(cl_object x, cl_object y);
extern ECL_API cl_object cl_rem(cl_object x, cl_object y);
extern ECL_API cl_object cl_decode_float(cl_object x);
extern ECL_API cl_object cl_scale_float(cl_object x, cl_object y);
extern ECL_API cl_object cl_float_radix(cl_object x);
extern ECL_API cl_object cl_float_digits(cl_object x);
extern ECL_API cl_object cl_float_precision(cl_object x);
extern ECL_API cl_object cl_integer_decode_float(cl_object x);
extern ECL_API cl_object cl_realpart(cl_object x);
extern ECL_API cl_object cl_imagpart(cl_object x);
extern ECL_API cl_object cl_float _ECL_ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_floor _ECL_ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_ceiling _ECL_ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_truncate _ECL_ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_round _ECL_ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_float_sign _ECL_ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_complex _ECL_ARGS((cl_narg narg, cl_object r, ...));

extern ECL_API cl_object ecl_floor1(cl_object x);
extern ECL_API cl_object ecl_ceiling1(cl_object x);
extern ECL_API cl_object ecl_truncate1(cl_object x);
extern ECL_API cl_object ecl_round1(cl_object x);
extern ECL_API int ecl_signbit(cl_object x);
extern ECL_API cl_object ecl_floor2(cl_object x, cl_object y);
extern ECL_API cl_object ecl_ceiling2(cl_object x, cl_object y);
extern ECL_API cl_object ecl_truncate2(cl_object x, cl_object y);
extern ECL_API cl_object ecl_round2(cl_object x, cl_object y);


/* num_comp.c */

extern ECL_API cl_object cl_E _ECL_ARGS((cl_narg narg, cl_object num, ...));
extern ECL_API cl_object cl_NE _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_L _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_G _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_GE _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_LE _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_max _ECL_ARGS((cl_narg narg, cl_object max, ...));
extern ECL_API cl_object cl_min _ECL_ARGS((cl_narg narg, cl_object min, ...));

extern ECL_API int ecl_number_equalp(cl_object x, cl_object y);
extern ECL_API int ecl_number_compare(cl_object x, cl_object y);
#define ecl_lowereq(x,y) (ecl_number_compare((x),(y)) <= 0)
#define ecl_greatereq(x,y) (ecl_number_compare((x),(y)) >= 0)
#define ecl_lower(x,y) (ecl_number_compare((x),(y)) < 0)
#define ecl_greater(x,y) (ecl_number_compare((x),(y)) > 0)

/* num_log.c */

#define ECL_BOOLCLR		0
#define ECL_BOOLAND		01
#define ECL_BOOLANDC2		02
#define ECL_BOOL1		03
#define ECL_BOOLANDC1		04
#define ECL_BOOL2		05
#define ECL_BOOLXOR		06
#define ECL_BOOLIOR		07
#define ECL_BOOLNOR		010
#define ECL_BOOLEQV		011
#define ECL_BOOLC2		012
#define ECL_BOOLORC2		013
#define ECL_BOOLC1		014
#define ECL_BOOLORC1		015
#define ECL_BOOLNAND		016
#define ECL_BOOLSET		017

extern ECL_API cl_object cl_lognand(cl_object x, cl_object y);
extern ECL_API cl_object cl_lognor(cl_object x, cl_object y);
extern ECL_API cl_object cl_logandc1(cl_object x, cl_object y);
extern ECL_API cl_object cl_logandc2(cl_object x, cl_object y);
extern ECL_API cl_object cl_logorc1(cl_object x, cl_object y);
extern ECL_API cl_object cl_logorc2(cl_object x, cl_object y);
extern ECL_API cl_object cl_lognot(cl_object x);
extern ECL_API cl_object cl_boole(cl_object o, cl_object x, cl_object y);
extern ECL_API cl_object cl_logbitp(cl_object p, cl_object x);
extern ECL_API cl_object cl_ash(cl_object x, cl_object y);
extern ECL_API cl_object cl_logcount(cl_object x);
extern ECL_API cl_object cl_integer_length(cl_object x);
extern ECL_API cl_object si_bit_array_op(cl_object o, cl_object x, cl_object y, cl_object r);
extern ECL_API cl_object cl_logior _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_logxor _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_logand _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_logeqv _ECL_ARGS((cl_narg narg, ...));

extern ECL_API cl_fixnum ecl_logand_index(cl_object n, cl_index i);
extern ECL_API cl_object ecl_boole(int op, cl_object x, cl_object y);
extern ECL_API cl_object ecl_ash(cl_object x, cl_fixnum w);
extern ECL_API int ecl_fixnum_bit_length(cl_fixnum l);
extern ECL_API cl_index ecl_integer_length(cl_object i);

/* num_pred.c */

extern ECL_API cl_object cl_zerop(cl_object x);
extern ECL_API cl_object cl_plusp(cl_object x);
extern ECL_API cl_object cl_minusp(cl_object x);
extern ECL_API cl_object cl_oddp(cl_object x);
extern ECL_API cl_object cl_evenp(cl_object x);
extern ECL_API cl_object si_float_nan_p(cl_object x);
extern ECL_API cl_object si_float_infinity_p(cl_object x);

extern ECL_API int ecl_zerop(cl_object x);
extern ECL_API int ecl_plusp(cl_object x);
extern ECL_API int ecl_minusp(cl_object x);
extern ECL_API int ecl_oddp(cl_object x);
extern ECL_API int ecl_evenp(cl_object x);
extern ECL_API bool ecl_float_nan_p(cl_object x);
extern ECL_API bool ecl_float_infinity_p(cl_object x);


/* num_rand.c */

extern ECL_API cl_object cl_random_state_p(cl_object x);
extern ECL_API cl_object cl_random _ECL_ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_make_random_state _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object ecl_make_random_state(cl_object rs);


/* num_sfun.c */

extern ECL_API cl_fixnum ecl_fixnum_expt(cl_fixnum x, cl_fixnum y);
extern ECL_API cl_object cl_abs(cl_object x);
extern ECL_API cl_object cl_exp(cl_object x);
extern ECL_API cl_object cl_expt(cl_object x, cl_object y);
extern ECL_API cl_object cl_sqrt(cl_object x);
extern ECL_API cl_object cl_sin(cl_object x);
extern ECL_API cl_object cl_cos(cl_object x);
extern ECL_API cl_object cl_tan(cl_object x);
extern ECL_API cl_object cl_sinh(cl_object x);
extern ECL_API cl_object cl_cosh(cl_object x);
extern ECL_API cl_object cl_tanh(cl_object x);
extern ECL_API cl_object cl_atan _ECL_ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_log _ECL_ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object si_log1p(cl_object x);

extern ECL_API cl_object ecl_log1p(cl_object x);
extern ECL_API cl_object ecl_log1(cl_object x);
extern ECL_API cl_object ecl_log2(cl_object x, cl_object y);
extern ECL_API cl_object ecl_atan2(cl_object y, cl_object x);
extern ECL_API cl_object ecl_atan1(cl_object y);

extern ECL_API cl_object ecl_abs(cl_object x);
extern ECL_API cl_object ecl_exp(cl_object x);
extern ECL_API cl_object ecl_expt(cl_object x, cl_object y);
extern ECL_API cl_object ecl_sqrt(cl_object x);
extern ECL_API cl_object ecl_sin(cl_object x);
extern ECL_API cl_object ecl_cos(cl_object x);
extern ECL_API cl_object ecl_tan(cl_object x);
extern ECL_API cl_object ecl_sinh(cl_object x);
extern ECL_API cl_object ecl_cosh(cl_object x);
extern ECL_API cl_object ecl_tanh(cl_object x);

/* package.c */

extern ECL_API void CEpackage_error(const char *message, const char *continue_message, cl_object package, int narg, ...);
extern ECL_API cl_object si_select_package(cl_object pack_name);
extern ECL_API cl_object cl_find_package(cl_object p);
extern ECL_API cl_object cl_package_name(cl_object p);
extern ECL_API cl_object cl_package_nicknames(cl_object p);
extern ECL_API cl_object cl_package_use_list(cl_object p);
extern ECL_API cl_object cl_package_used_by_list(cl_object p);
extern ECL_API cl_object cl_package_shadowing_symbols(cl_object p);
extern ECL_API cl_object cl_list_all_packages(void);
extern ECL_API cl_object si_package_hash_tables(cl_object p);
extern ECL_API cl_object si_package_lock(cl_object p, cl_object t);
extern ECL_API cl_object cl_delete_package(cl_object p);
extern ECL_API cl_object cl_make_package _ECL_ARGS((cl_narg narg, cl_object pack_name, ...));
extern ECL_API cl_object cl_intern _ECL_ARGS((cl_narg narg, cl_object strng, ...));
extern ECL_API cl_object cl_find_symbol _ECL_ARGS((cl_narg narg, cl_object strng, ...));
extern ECL_API cl_object cl_unintern _ECL_ARGS((cl_narg narg, cl_object symbl, ...));
extern ECL_API cl_object cl_export _ECL_ARGS((cl_narg narg, cl_object symbols, ...));
extern ECL_API cl_object cl_unexport _ECL_ARGS((cl_narg narg, cl_object symbols, ...));
extern ECL_API cl_object cl_import _ECL_ARGS((cl_narg narg, cl_object symbols, ...));
extern ECL_API cl_object cl_rename_package _ECL_ARGS((cl_narg narg, cl_object pack, cl_object new_name, ...));
extern ECL_API cl_object cl_shadowing_import _ECL_ARGS((cl_narg narg, cl_object symbols, ...));
extern ECL_API cl_object cl_shadow _ECL_ARGS((cl_narg narg, cl_object symbols, ...));
extern ECL_API cl_object cl_use_package _ECL_ARGS((cl_narg narg, cl_object pack, ...));
extern ECL_API cl_object cl_unuse_package _ECL_ARGS((cl_narg narg, cl_object pack, ...));

extern ECL_API cl_object ecl_make_package(cl_object n, cl_object ns, cl_object ul);
extern ECL_API cl_object ecl_rename_package(cl_object x, cl_object n, cl_object ns);
extern ECL_API cl_object ecl_find_package_nolock(cl_object n);
extern ECL_API cl_object ecl_find_package(const char *p);
extern ECL_API cl_object si_coerce_to_package(cl_object p);
extern ECL_API cl_object ecl_current_package(void);
extern ECL_API cl_object ecl_find_symbol(cl_object n, cl_object p, int *intern_flag);
extern ECL_API cl_object ecl_intern(cl_object name, cl_object p, int *intern_flag);
extern ECL_API cl_object _ecl_intern(const char *s, cl_object p);
extern ECL_API bool ecl_unintern(cl_object s, cl_object p);
extern ECL_API void cl_export2(cl_object s, cl_object p);
extern ECL_API void cl_unexport2(cl_object s, cl_object p);
extern ECL_API void cl_import2(cl_object s, cl_object p);
extern ECL_API void ecl_shadowing_import(cl_object s, cl_object p);
extern ECL_API void ecl_shadow(cl_object s, cl_object p);
extern ECL_API void ecl_use_package(cl_object x0, cl_object p);
extern ECL_API void ecl_unuse_package(cl_object x0, cl_object p);


/* pathname.c */

extern ECL_API bool ecl_string_match(cl_object s, cl_index j, cl_index ls, cl_object p, cl_index i, cl_index lp);

extern ECL_API cl_object cl_pathname(cl_object name);
extern ECL_API cl_object cl_logical_pathname(cl_object pname);
extern ECL_API cl_object cl_pathnamep(cl_object pname);
extern ECL_API cl_object cl_pathname_host _ECL_ARGS((cl_narg narg, cl_object pname, ...));
extern ECL_API cl_object cl_pathname_device _ECL_ARGS((cl_narg narg, cl_object pname, ...));
extern ECL_API cl_object cl_pathname_directory _ECL_ARGS((cl_narg narg, cl_object pname, ...));
extern ECL_API cl_object cl_pathname_name _ECL_ARGS((cl_narg narg, cl_object pname, ...));
extern ECL_API cl_object cl_pathname_type _ECL_ARGS((cl_narg narg, cl_object pname, ...));
extern ECL_API cl_object cl_pathname_version(cl_object pname);
extern ECL_API cl_object cl_namestring(cl_object pname);
extern ECL_API cl_object cl_file_namestring(cl_object pname);
extern ECL_API cl_object cl_directory_namestring(cl_object pname);
extern ECL_API cl_object cl_host_namestring(cl_object pname);
extern ECL_API cl_object si_logical_pathname_p(cl_object pname);
extern ECL_API cl_object cl_pathname_match_p(cl_object path, cl_object mask);
extern ECL_API cl_object cl_translate_pathname _ECL_ARGS((cl_narg narg, cl_object source, cl_object from, cl_object to, ...));
extern ECL_API cl_object cl_translate_logical_pathname _ECL_ARGS((cl_narg narg, cl_object source, ...));
extern ECL_API cl_object cl_parse_namestring _ECL_ARGS((cl_narg narg, cl_object thing, ...));
extern ECL_API cl_object cl_parse_logical_namestring _ECL_ARGS((cl_narg narg, cl_object thing, ...));
extern ECL_API cl_object cl_merge_pathnames _ECL_ARGS((cl_narg narg, cl_object path, ...));
extern ECL_API cl_object cl_make_pathname _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_enough_namestring _ECL_ARGS((cl_narg narg, cl_object path, ...));
extern ECL_API cl_object si_pathname_translations _ECL_ARGS((cl_narg narg, cl_object host, ...));
extern ECL_API cl_object si_default_pathname_defaults(void);
extern ECL_API cl_object cl_wild_pathname_p _ECL_ARGS((cl_narg narg, cl_object pathname, ...));

extern ECL_API cl_object ecl_make_pathname(cl_object host, cl_object device, cl_object directory, cl_object name, cl_object type, cl_object version, cl_object scase);
extern ECL_API cl_object ecl_parse_namestring(cl_object s, cl_index start, cl_index end, cl_index *ep, cl_object default_host);
extern ECL_API cl_object coerce_to_physical_pathname(cl_object x);
extern ECL_API cl_object coerce_to_file_pathname(cl_object pathname);
#define ECL_NAMESTRING_TRUNCATE_IF_ERROR 1
#define ECL_NAMESTRING_FORCE_BASE_STRING 2
extern ECL_API cl_object ecl_namestring(cl_object pname, int truncate_if_impossible);
extern ECL_API cl_object si_coerce_to_filename(cl_object pathname);
extern ECL_API cl_object ecl_merge_pathnames(cl_object path, cl_object defaults, cl_object default_version);
extern ECL_API bool ecl_logical_hostname_p(cl_object host);


/* predicate.c */

extern ECL_API cl_object cl_identity(cl_object x);
extern ECL_API cl_object cl_null(cl_object x);
#define cl_not cl_null
extern ECL_API cl_object cl_symbolp(cl_object x);
extern ECL_API cl_object cl_atom(cl_object x);
extern ECL_API cl_object cl_consp(cl_object x);
extern ECL_API cl_object cl_listp(cl_object x);
extern ECL_API cl_object cl_numberp(cl_object x);
extern ECL_API cl_object cl_integerp(cl_object x);
extern ECL_API cl_object cl_rationalp(cl_object x);
extern ECL_API cl_object cl_floatp(cl_object x);
extern ECL_API cl_object cl_realp(cl_object x);
extern ECL_API cl_object cl_complexp(cl_object x);
extern ECL_API cl_object cl_characterp(cl_object x);
extern ECL_API cl_object cl_stringp(cl_object x);
extern ECL_API cl_object cl_bit_vector_p(cl_object x);
extern ECL_API cl_object cl_vectorp(cl_object x);
extern ECL_API cl_object cl_simple_string_p(cl_object x);
extern ECL_API cl_object cl_simple_bit_vector_p(cl_object x);
extern ECL_API cl_object cl_simple_vector_p(cl_object x);
extern ECL_API cl_object cl_arrayp(cl_object x);
extern ECL_API cl_object cl_packagep(cl_object x);
extern ECL_API cl_object cl_functionp(cl_object x);
extern ECL_API cl_object cl_compiled_function_p(cl_object x);
extern ECL_API cl_object cl_eq(cl_object x, cl_object y);
extern ECL_API cl_object cl_eql(cl_object x, cl_object y);
extern ECL_API cl_object cl_equal(cl_object x, cl_object y);
extern ECL_API cl_object cl_equalp(cl_object x, cl_object y);
extern ECL_API cl_object si_fixnump(cl_object x);

extern ECL_API bool floatp(cl_object x);
extern ECL_API bool ecl_numberp(cl_object x);
extern ECL_API bool ecl_realp(cl_object x);
extern ECL_API bool ecl_eql(cl_object x, cl_object y);
extern ECL_API bool ecl_equal(register cl_object x, cl_object y);
extern ECL_API bool ecl_equalp(cl_object x, cl_object y);
extern ECL_API bool ecl_stringp(cl_object x);

/* print.c */

extern ECL_API cl_object cl_write_byte(cl_object integer, cl_object binary_output_stream);
extern ECL_API cl_object cl_write_sequence _ECL_ARGS((cl_narg narg, cl_object seq, cl_object stream, ...));
extern ECL_API cl_object cl_write _ECL_ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_prin1 _ECL_ARGS((cl_narg narg, cl_object obj, ...));
extern ECL_API cl_object cl_print _ECL_ARGS((cl_narg narg, cl_object obj, ...));
extern ECL_API cl_object cl_pprint _ECL_ARGS((cl_narg narg, cl_object obj, ...));
extern ECL_API cl_object cl_princ _ECL_ARGS((cl_narg narg, cl_object obj, ...));
extern ECL_API cl_object cl_write_char _ECL_ARGS((cl_narg narg, cl_object c, ...));
extern ECL_API cl_object cl_write_string _ECL_ARGS((cl_narg narg, cl_object strng, ...));
extern ECL_API cl_object cl_write_line _ECL_ARGS((cl_narg narg, cl_object strng, ...));
extern ECL_API cl_object cl_terpri _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_finish_output _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_fresh_line _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_force_output _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_clear_output _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_write_object(cl_object object, cl_object stream);
extern ECL_API cl_object si_write_ugly_object(cl_object object, cl_object stream);

extern ECL_API cl_object ecl_princ(cl_object obj, cl_object strm);
extern ECL_API cl_object ecl_prin1(cl_object obj, cl_object strm);
extern ECL_API cl_object ecl_print(cl_object obj, cl_object strm);
extern ECL_API cl_object ecl_terpri(cl_object strm);
extern ECL_API void ecl_write_string(cl_object strng, cl_object strm);
extern ECL_API void ecl_princ_str(const char *s, cl_object sym);
extern ECL_API int ecl_princ_char(int c, cl_object sym);

extern ECL_API cl_fixnum ecl_print_level(void);
extern ECL_API cl_fixnum ecl_print_length(void);
extern ECL_API int ecl_print_base(void);
extern ECL_API bool ecl_print_radix(void);
extern ECL_API cl_object ecl_print_case(void);
extern ECL_API bool ecl_print_gensym(void);
extern ECL_API bool ecl_print_array(void);
extern ECL_API bool ecl_print_readably(void);
extern ECL_API bool ecl_print_escape(void);
extern ECL_API bool ecl_print_circle(void);

/* printer/integer_to_string.d */
extern ECL_API cl_object si_integer_to_string(cl_object buffer, cl_object integer, cl_object base, cl_object radix, cl_object decimalp);

/* printer/float_string.d */
extern ECL_API cl_object si_float_string(cl_narg narg, cl_object string, cl_object x, ...);

/* printer/float_to_digits.d */
extern ECL_API cl_object si_float_to_digits(cl_object digits, cl_object number, cl_object position, cl_object relativep);

/* printer/float_to_string.d */
extern ECL_API cl_object si_float_to_string_free(cl_object buffer, cl_object number, cl_object e_min, cl_object e_max);

/* printer/print_unreadable.d */
extern ECL_API cl_object si_print_unreadable_object_function(cl_object o, cl_object stream, cl_object type, cl_object id, cl_object fn);

/* profile.c */
#ifdef PROFILE
extern ECL_API cl_object si_profile _ECL_ARGS((cl_narg narg, cl_object scale, cl_object start_address));
extern ECL_API cl_object si_clear_profile _ECL_ARGS((cl_narg narg));
extern ECL_API cl_object si_display_profile _ECL_ARGS((cl_narg narg));
extern ECL_API int total_ticks(unsigned short *aar, unsigned int dim);
extern ECL_API int init_profile(void);
#endif


/* read.c */

extern ECL_API cl_object si_get_buffer_string();
extern ECL_API cl_object si_put_buffer_string(cl_object string);
extern ECL_API cl_object cl_read_sequence _ECL_ARGS((cl_narg narg, cl_object seq, cl_object stream, ...));
extern ECL_API cl_object cl_readtablep(cl_object readtable);
extern ECL_API cl_object si_standard_readtable(void);
extern ECL_API cl_object cl_read _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_read_preserving_whitespace _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_read_delimited_list _ECL_ARGS((cl_narg narg, cl_object d, ...));
extern ECL_API cl_object cl_read_line _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_read_char _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_unread_char _ECL_ARGS((cl_narg narg, cl_object c, ...));
extern ECL_API cl_object cl_peek_char _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_listen _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_read_char_no_hang _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_clear_input _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_parse_integer _ECL_ARGS((cl_narg narg, cl_object strng, ...));
extern ECL_API cl_object cl_read_byte _ECL_ARGS((cl_narg narg, cl_object binary_input_stream, ...));
extern ECL_API cl_object cl_copy_readtable _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_readtable_case(cl_object r);
extern ECL_API cl_object si_readtable_case_set(cl_object r, cl_object mode);
extern ECL_API cl_object cl_set_syntax_from_char _ECL_ARGS((cl_narg narg, cl_object tochr, cl_object fromchr, ...));
extern ECL_API cl_object cl_set_macro_character _ECL_ARGS((cl_narg narg, cl_object chr, cl_object fnc, ...));
extern ECL_API cl_object cl_get_macro_character _ECL_ARGS((cl_narg narg, cl_object chr, ...));
extern ECL_API cl_object cl_make_dispatch_macro_character _ECL_ARGS((cl_narg narg, cl_object chr, ...));
extern ECL_API cl_object cl_set_dispatch_macro_character _ECL_ARGS((cl_narg narg, cl_object dspchr, cl_object subchr, cl_object fnc, ...));
extern ECL_API cl_object cl_get_dispatch_macro_character _ECL_ARGS((cl_narg narg, cl_object dspchr, cl_object subchr, ...));
extern ECL_API cl_object si_read_object_or_ignore(cl_object stream, cl_object eof);
extern ECL_API cl_object si_readtable_lock _ECL_ARGS((cl_narg narg, cl_object readtable, ...));

extern ECL_API int ecl_readtable_get(cl_object rdtbl, int c, cl_object *macro);
extern ECL_API void ecl_readtable_set(cl_object rdtbl, int c, enum ecl_chattrib cat, cl_object macro_or_table);
extern ECL_API cl_object ecl_read_object_non_recursive(cl_object in);
extern ECL_API cl_object ecl_read_object(cl_object in);
extern ECL_API cl_object ecl_parse_number(cl_object s, cl_index start, cl_index end, cl_index *ep, unsigned int radix);
extern ECL_API cl_object ecl_parse_integer(cl_object s, cl_index start, cl_index end, cl_index *ep, unsigned int radix);
extern ECL_API bool ecl_invalid_character_p(int c);
extern ECL_API cl_object ecl_copy_readtable(cl_object from, cl_object to);
extern ECL_API cl_object ecl_current_readtable(void);
extern ECL_API int ecl_current_read_base(void);
extern ECL_API char ecl_current_read_default_float_format(void);
#define ecl_read_from_cstring(s) si_string_to_object(1,make_constant_base_string(s))
#define ecl_read_from_cstring_safe(s,v) si_string_to_object(2,make_constant_base_string(s),(v))
extern ECL_API cl_object ecl_init_module(cl_object block, void (*entry)(cl_object));

/* reference.c */

extern ECL_API cl_object cl_fboundp(cl_object sym);
extern ECL_API cl_object cl_symbol_function(cl_object sym);
extern ECL_API cl_object cl_fdefinition(cl_object fname);
extern ECL_API cl_object si_coerce_to_function(cl_object form);
extern ECL_API cl_object cl_symbol_value(cl_object sym);
extern ECL_API cl_object cl_boundp(cl_object sym);
extern ECL_API cl_object cl_special_operator_p(cl_object form);
extern ECL_API cl_object ecl_fdefinition(cl_object fname);
extern ECL_API bool ecl_boundp(cl_env_ptr env, cl_object o);

/* sequence.c */

extern ECL_API cl_object si_sequence_start_end(cl_object fun, cl_object sequence, cl_object start, cl_object end);
extern ECL_API cl_object cl_elt(cl_object x, cl_object i);
extern ECL_API cl_object si_elt_set(cl_object seq, cl_object index, cl_object val);
extern ECL_API cl_object cl_copy_seq(cl_object x);
extern ECL_API cl_object cl_length(cl_object x);
extern ECL_API cl_object cl_reverse(cl_object x);
extern ECL_API cl_object cl_nreverse(cl_object x);
extern ECL_API cl_object cl_subseq _ECL_ARGS((cl_narg narg, cl_object sequence, cl_object start, ...));

extern ECL_API cl_object ecl_elt(cl_object seq, cl_fixnum index);
extern ECL_API cl_object ecl_elt_set(cl_object seq, cl_fixnum index, cl_object val);
extern ECL_API cl_fixnum ecl_length(cl_object x);
extern ECL_API cl_object ecl_subseq(cl_object seq, cl_index start, cl_index limit);
extern ECL_API cl_object ecl_copy_seq(cl_object seq);

#ifdef ECL_SSE2
/* sse2.c */

extern ECL_API cl_object si_sse_pack_p(cl_object x);
extern ECL_API cl_object si_sse_pack_as_elt_type(cl_object x, cl_object type);
extern ECL_API cl_object si_sse_pack_element_type(cl_object x);

extern ECL_API cl_object si_vector_to_sse_pack(cl_object x);
extern ECL_API cl_object si_sse_pack_to_vector(cl_object x, cl_object elt_type);

extern ECL_API cl_object ecl_make_int_sse_pack(__m128i value);
extern ECL_API __m128i ecl_unbox_int_sse_pack(cl_object value);
#define ecl_unbox_int_sse_pack_unsafe(x) ((x)->sse.data.vi)

extern ECL_API cl_object ecl_make_float_sse_pack(__m128 value);
extern ECL_API __m128 ecl_unbox_float_sse_pack(cl_object value);
#define ecl_unbox_float_sse_pack_unsafe(x) ((x)->sse.data.vf)

extern ECL_API cl_object ecl_make_double_sse_pack(__m128d value);
extern ECL_API __m128d ecl_unbox_double_sse_pack(cl_object value);
#define ecl_unbox_double_sse_pack_unsafe(x) ((x)->sse.data.vd)

#endif

/* stacks.c */

extern ECL_API cl_object si_ihs_top(void);
extern ECL_API cl_object si_ihs_fun(cl_object arg);
extern ECL_API cl_object si_ihs_env(cl_object arg);
extern ECL_API cl_object si_ihs_bds(cl_object arg);
extern ECL_API cl_object si_ihs_next(cl_object arg);
extern ECL_API cl_object si_ihs_prev(cl_object arg);
extern ECL_API cl_object si_frs_top(void);
extern ECL_API cl_object si_frs_bds(cl_object arg);
extern ECL_API cl_object si_frs_tag(cl_object arg);
extern ECL_API cl_object si_frs_ihs(cl_object arg);
extern ECL_API cl_object si_bds_top(void);
extern ECL_API cl_object si_bds_var(cl_object arg);
extern ECL_API cl_object si_bds_val(cl_object arg);
extern ECL_API cl_object si_sch_frs_base(cl_object fr, cl_object ihs);
extern ECL_API cl_object si_reset_stack_limits(void);
extern ECL_API cl_object si_set_limit(cl_object type, cl_object size);
extern ECL_API cl_object si_get_limit(cl_object type);

extern ECL_API cl_index ecl_progv(cl_env_ptr env, cl_object vars, cl_object values);
extern ECL_API void ecl_bds_unwind(cl_env_ptr env, cl_index new_bds_top_index);
extern ECL_API void ecl_unwind(cl_env_ptr env, struct ecl_frame *fr) /*ecl_attr_noreturn*/;
extern ECL_API struct ecl_frame *frs_sch(cl_object frame_id);

/* string.c */

extern ECL_API cl_object cl_char(cl_object s, cl_object i);
#define cl_schar(x,y) cl_char(x,y)
extern ECL_API cl_object si_char_set(cl_object str, cl_object index, cl_object c);
extern ECL_API cl_object cl_string_trim(cl_object char_bag, cl_object strng);
extern ECL_API cl_object cl_string_left_trim(cl_object char_bag, cl_object strng);
extern ECL_API cl_object cl_string_right_trim(cl_object char_bag, cl_object strng);
extern ECL_API cl_object cl_string(cl_object x);
extern ECL_API cl_object cl_make_string _ECL_ARGS((cl_narg narg, cl_object size, ...));
extern ECL_API cl_object cl_stringE _ECL_ARGS((cl_narg narg, cl_object string1, cl_object string2, ...));
extern ECL_API cl_object cl_string_equal _ECL_ARGS((cl_narg narg, cl_object string1, cl_object string2, ...));
extern ECL_API cl_object cl_stringL _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_stringG _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_stringLE _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_stringGE _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_stringNE _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_string_lessp _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_string_greaterp _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_string_not_greaterp _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_string_not_lessp _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_string_not_equal _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_string_upcase _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_string_downcase _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_string_capitalize _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_nstring_upcase _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_nstring_downcase _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_nstring_capitalize _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_base_string_concatenate _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_copy_to_simple_base_string(cl_object s);

#define ecl_alloc_simple_base_string(l) ecl_alloc_simple_vector((l),ecl_aet_bc)
extern ECL_API cl_object ecl_alloc_adjustable_base_string(cl_index l);
extern ECL_API cl_object ecl_make_simple_base_string(char *s, cl_fixnum i);
#define ecl_make_constant_base_string(s,n) ecl_make_simple_base_string((char*)s,n)
extern ECL_API cl_object make_base_string_copy(const char *s);
extern ECL_API cl_object ecl_cstring_to_base_string_or_nil(const char *s);
extern ECL_API bool ecl_string_eq(cl_object x, cl_object y);
extern ECL_API bool ecl_member_char(ecl_character c, cl_object char_bag);
extern ECL_API bool ecl_fits_in_base_string(cl_object s);
extern ECL_API ecl_character ecl_char(cl_object s, cl_index i);
extern ECL_API ecl_character ecl_char_set(cl_object s, cl_index i, ecl_character c);

/* structure.c */

extern ECL_API cl_object si_structure_subtype_p(cl_object x, cl_object y);
extern ECL_API cl_object cl_copy_structure(cl_object s);
extern ECL_API cl_object si_structure_name(cl_object s);
extern ECL_API cl_object si_structure_ref(cl_object x, cl_object type, cl_object index);
extern ECL_API cl_object si_structure_set(cl_object x, cl_object type, cl_object index, cl_object val);
extern ECL_API cl_object si_structurep(cl_object s);
extern ECL_API cl_object si_make_structure _ECL_ARGS((cl_narg narg, cl_object type, ...));

#ifndef CLOS
extern ECL_API cl_object structure_to_list(cl_object x);
#endif
extern ECL_API cl_object ecl_structure_ref(cl_object x, cl_object name, int n);
extern ECL_API cl_object ecl_structure_set(cl_object x, cl_object name, int n, cl_object v);


/* symbol.c */

extern ECL_API cl_object cl_make_symbol(cl_object str);
extern ECL_API cl_object cl_remprop(cl_object sym, cl_object prop);
extern ECL_API cl_object cl_symbol_plist(cl_object sym);
extern ECL_API cl_object cl_get_properties(cl_object place, cl_object indicator_list);
extern ECL_API cl_object cl_symbol_name(cl_object sym);
extern ECL_API cl_object cl_symbol_package(cl_object sym);
extern ECL_API cl_object cl_keywordp(cl_object sym);
extern ECL_API cl_object si_put_f(cl_object plist, cl_object value, cl_object indicator);
extern ECL_API cl_object si_rem_f(cl_object plist, cl_object indicator);
extern ECL_API cl_object si_set_symbol_plist(cl_object sym, cl_object plist);
extern ECL_API cl_object si_putprop(cl_object sym, cl_object value, cl_object indicator);
extern ECL_API cl_object si_Xmake_special(cl_object sym);
extern ECL_API cl_object si_Xmake_constant(cl_object sym, cl_object val);
extern ECL_API cl_object cl_get _ECL_ARGS((cl_narg narg, cl_object sym, cl_object indicator, ...));
extern ECL_API cl_object cl_getf _ECL_ARGS((cl_narg narg, cl_object place, cl_object indicator, ...));
extern ECL_API cl_object cl_copy_symbol _ECL_ARGS((cl_narg narg, cl_object sym, ...));
extern ECL_API cl_object cl_gensym _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_gentemp _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_put_properties _ECL_ARGS((cl_narg narg, cl_object sym, ...));

extern ECL_API void ecl_defvar(cl_object s, cl_object v);
extern ECL_API void ecl_defparameter(cl_object s, cl_object v);
extern ECL_API cl_object ecl_make_symbol(const char *s, const char*p);
extern ECL_API cl_object ecl_make_keyword(const char *s);
extern ECL_API cl_object ecl_symbol_value(cl_object s);
extern ECL_API cl_object ecl_setq(cl_env_ptr env, cl_object var, cl_object value);
extern ECL_API cl_object ecl_symbol_name(cl_object s);
extern ECL_API cl_object ecl_symbol_package(cl_object s);
extern ECL_API int ecl_symbol_type(cl_object s);
extern ECL_API void ecl_symbol_type_set(cl_object s, int t);
extern ECL_API cl_object ecl_getf(cl_object place, cl_object indicator, cl_object deflt);
extern ECL_API cl_object ecl_get(cl_object s, cl_object p, cl_object d);
extern ECL_API bool ecl_keywordp(cl_object s);


/* tcp.c */

#ifdef TCP
extern ECL_API cl_object si_open_client_stream(cl_object host, cl_object port);
extern ECL_API cl_object si_open_server_stream(cl_object port);
extern ECL_API cl_object si_open_unix_socket_stream(cl_object path);
extern ECL_API cl_object si_lookup_host_entry(cl_object host_or_address);
extern ECL_API void ecl_tcp_close_all(void);
#endif


/* threads.c */

#ifdef ECL_THREADS
extern ECL_API cl_object mp_own_process(void) __attribute__((const));
extern ECL_API cl_object mp_all_processes(void);
extern ECL_API cl_object mp_exit_process(void) ecl_attr_noreturn;
extern ECL_API cl_object mp_interrupt_process(cl_object process, cl_object function);
extern ECL_API cl_object mp_make_process _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object mp_process_active_p(cl_object process);
extern ECL_API cl_object mp_process_enable(cl_object process);
extern ECL_API cl_object mp_process_yield(void);
extern ECL_API cl_object mp_process_join(cl_object process);
extern ECL_API cl_object mp_process_interrupt(cl_object process, cl_object function);
extern ECL_API cl_object mp_process_kill(cl_object process);
extern ECL_API cl_object mp_process_suspend(cl_object process);
extern ECL_API cl_object mp_process_resume(cl_object process);
extern ECL_API cl_object mp_process_name(cl_object process);
extern ECL_API cl_object mp_process_preset _ECL_ARGS((cl_narg narg, cl_object process, cl_object function, ...));
extern ECL_API cl_object mp_process_run_function _ECL_ARGS((cl_narg narg, cl_object name, cl_object function, ...));
extern ECL_API cl_object mp_process_run_function_wait _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object mp_process_whostate(cl_object process);
extern ECL_API cl_object mp_make_condition_variable(void);
extern ECL_API cl_object mp_condition_variable_wait(cl_object cv, cl_object lock);
extern ECL_API cl_object mp_condition_variable_timedwait(cl_object cv, cl_object lock, cl_object seconds);
extern ECL_API cl_object mp_condition_variable_signal(cl_object cv);
extern ECL_API cl_object mp_condition_variable_broadcast(cl_object cv);
extern ECL_API cl_object mp_current_process(void);
extern ECL_API cl_object mp_block_signals(void);
extern ECL_API cl_object mp_restore_signals(cl_object sigmask);

extern ECL_API bool ecl_import_current_thread(cl_object process_name, cl_object process_binding);
extern ECL_API void ecl_release_current_thread(void);

/* threads/semaphore.d */

extern ECL_API cl_object mp_make_semaphore _ECL_ARGS((cl_narg, ...));
extern ECL_API cl_object mp_semaphore_count(cl_object);
extern ECL_API cl_object mp_semaphore_name(cl_object);
extern ECL_API cl_object mp_semaphore_wait_count(cl_object);
extern ECL_API cl_object mp_wait_on_semaphore(cl_object);
extern ECL_API cl_object mp_signal_semaphore _ECL_ARGS((cl_narg, cl_object, ...));
extern ECL_API cl_object ecl_make_semaphore(cl_object name, cl_fixnum count);

/* threads/barrier.d */

extern ECL_API cl_object ecl_make_barrier(cl_object name, cl_index count);
extern ECL_API cl_object mp_make_barrier _ECL_ARGS((cl_narg, cl_object, ...));
extern ECL_API cl_object mp_barrier_count(cl_object);
extern ECL_API cl_object mp_barrier_name(cl_object);
extern ECL_API cl_object mp_barrier_arrivers_count(cl_object);
extern ECL_API cl_object mp_barrier_wait _ECL_ARGS((cl_narg, cl_object, ...));
extern ECL_API cl_object mp_barrier_unblock _ECL_ARGS((cl_narg, cl_object, ...));

/* threads/mailbox.d */

extern ECL_API cl_object mp_make_mailbox _ECL_ARGS((cl_narg, ...));
extern ECL_API cl_object mp_mailbox_name(cl_object mailbox);
extern ECL_API cl_object mp_mailbox_count(cl_object mailbox);
extern ECL_API cl_object mp_mailbox_empty_p(cl_object);
extern ECL_API cl_object mp_mailbox_read(cl_object mailbox);
extern ECL_API cl_object mp_mailbox_send(cl_object mailbox, cl_object msg);

/* threads/atomic.c */

extern ECL_API cl_object ecl_atomic_get(cl_object *slot);
extern ECL_API void ecl_atomic_push(cl_object *slot, cl_object o);
extern ECL_API void ecl_atomic_nconc(cl_object l, cl_object *slot);
extern ECL_API cl_object ecl_atomic_pop(cl_object *slot);
extern ECL_API cl_index ecl_atomic_index_incf(cl_index *slot);

/* threads/mutex.c */

extern ECL_API cl_object mp_make_lock _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object mp_recursive_lock_p(cl_object lock);
extern ECL_API cl_object mp_lock_name(cl_object lock);
extern ECL_API cl_object mp_lock_owner(cl_object lock);
extern ECL_API cl_object mp_lock_count(cl_object lock);
extern ECL_API cl_object mp_get_lock _ECL_ARGS((cl_narg narg, cl_object lock, ...));
extern ECL_API cl_object mp_get_lock_wait(cl_object lock);
extern ECL_API cl_object mp_get_lock_nowait(cl_object lock);
extern ECL_API cl_object mp_giveup_lock(cl_object lock);

extern ECL_API cl_object ecl_make_lock(cl_object lock, bool recursive);

/* threads/rwlock.d */

extern ECL_API cl_object mp_make_rwlock _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object mp_rwlock_name(cl_object lock);
extern ECL_API cl_object mp_get_rwlock_read _ECL_ARGS((cl_narg narg, cl_object lock, ...));
extern ECL_API cl_object mp_get_rwlock_write _ECL_ARGS((cl_narg narg, cl_object lock, ...));
extern ECL_API cl_object mp_giveup_rwlock_read(cl_object lock);
extern ECL_API cl_object mp_giveup_rwlock_write(cl_object lock);
extern ECL_API cl_object ecl_make_rwlock(cl_object lock);

#endif /* ECL_THREADS */

/* time.c */

extern ECL_API cl_object cl_sleep(cl_object z);
extern ECL_API cl_object cl_get_internal_run_time(void);
extern ECL_API cl_object cl_get_internal_real_time(void);
extern ECL_API cl_object cl_get_universal_time(void);


/* typespec.c */

extern ECL_API void assert_type_integer(cl_object p);
extern ECL_API void assert_type_non_negative_integer(cl_object p);
extern ECL_API void assert_type_proper_list(cl_object p);
extern ECL_API cl_object cl_type_of(cl_object x);

extern ECL_API void FEtype_error_fixnum(cl_object x) ecl_attr_noreturn;
extern ECL_API void FEtype_error_size(cl_object x) ecl_attr_noreturn;
extern ECL_API void FEtype_error_cons(cl_object x) ecl_attr_noreturn;
extern ECL_API void FEtype_error_list(cl_object x) ecl_attr_noreturn;
extern ECL_API void FEtype_error_proper_list(cl_object x) ecl_attr_noreturn;
extern ECL_API void FEtype_error_sequence(cl_object x) ecl_attr_noreturn;
extern ECL_API void FEtype_error_vector(cl_object x) ecl_attr_noreturn;
extern ECL_API void FEcircular_list(cl_object x) ecl_attr_noreturn;
extern ECL_API void FEtype_error_index(cl_object seq, cl_fixnum ndx) ecl_attr_noreturn;
extern ECL_API void FEtype_error_array(cl_object x) ecl_attr_noreturn;
extern ECL_API void FEdivision_by_zero(cl_object x, cl_object y) ecl_attr_noreturn;
extern ECL_API cl_object ecl_type_error(cl_object function, const char *place, cl_object o, cl_object type);
extern ECL_API cl_object ecl_check_cl_type(cl_object fun, cl_object p, cl_type t);
extern ECL_API cl_object ecl_make_integer_type(cl_object min, cl_object max);

/* unixfsys.c */

extern ECL_API cl_object cl_truename(cl_object file);
extern ECL_API cl_object cl_rename_file _ECL_ARGS((cl_narg narg, cl_object old_obj, cl_object new_obj, ...));
extern ECL_API cl_object cl_delete_file(cl_object file);
extern ECL_API cl_object cl_probe_file(cl_object file);
extern ECL_API cl_object cl_file_write_date(cl_object file);
extern ECL_API cl_object cl_file_author(cl_object file);
extern ECL_API cl_object si_file_kind(cl_object pathname, cl_object follow_links);
extern ECL_API cl_object si_getcwd _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_getpid(void);
extern ECL_API cl_object si_getuid(void);
extern ECL_API cl_object si_chdir _ECL_ARGS((cl_narg narg, cl_object directory, ...));
extern ECL_API cl_object si_chmod(cl_object filename, cl_object mode);
extern ECL_API cl_object si_mkdir(cl_object directory, cl_object mode);
extern ECL_API cl_object cl_directory _ECL_ARGS((cl_narg narg, cl_object directory, ...));
extern ECL_API cl_object cl_user_homedir_pathname _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_mkstemp(cl_object templ);
extern ECL_API cl_object si_rmdir(cl_object directory);

extern ECL_API cl_object ecl_cstring_to_pathname(char *s);
extern ECL_API int ecl_backup_open(const char *filename, int option, int mode);
extern ECL_API cl_object ecl_file_len(int f);
extern ECL_API cl_object ecl_homedir_pathname(cl_object user);
extern ECL_API cl_object si_get_library_pathname(void);
extern ECL_API cl_object si_copy_file(cl_object orig, cl_object end);


/* unixint.c */

#ifdef ECL_USE_MPROTECT
#define ecl_disable_interrupts_env(env) ((env)->disable_interrupts=1)
#define ecl_enable_interrupts_env(env) ((env)->disable_interrupts=0)
#else
#define ecl_disable_interrupts_env(env) ((env)->disable_interrupts=1)
#define ecl_enable_interrupts_env(env) (((env)->disable_interrupts^=1) && (ecl_check_pending_interrupts(env),0))
#endif
#define ecl_clear_interrupts_env(env) ((env)->pendinginterrupts=0)
#define ecl_clear_interrupts() ecl_clear_interrupts(&cl_env)
#define ecl_disable_interrupts() ecl_disable_interrupts_env(&cl_env)
#define ecl_enable_interrupts() ecl_enable_interrupts_env(&cl_env)
#define ECL_PSEUDO_ATOMIC_ENV(env,stmt) (ecl_disable_interrupts_env(env),(stmt),ecl_enable_interrupts_env(env))
#define ECL_PSEUDO_ATOMIC(stmt) (ecl_disable_interrupts(),(stmt),ecl_enable_interrupts())
extern ECL_API cl_object si_handle_signal(cl_object signal, cl_object process);
extern ECL_API cl_object si_get_signal_handler(cl_object signal);
extern ECL_API cl_object si_set_signal_handler(cl_object signal, cl_object handler);
extern ECL_API cl_object si_catch_signal(cl_narg narg, cl_object signal, cl_object state, ...);
extern ECL_API cl_object si_check_pending_interrupts(void);
extern ECL_API cl_object si_disable_interrupts(void);
extern ECL_API cl_object si_enable_interrupts(void);
extern ECL_API cl_object si_trap_fpe(cl_object condition, cl_object flag);
#if defined(ECL_MS_WINDOWS_HOST)
extern ECL_API LONG WINAPI _ecl_w32_exception_filter(struct _EXCEPTION_POINTERS*);
#endif
extern ECL_API void ecl_check_pending_interrupts(cl_env_ptr env);

/* unixsys.c */

extern ECL_API cl_object si_system(cl_object cmd);
extern ECL_API cl_object si_make_pipe();
extern ECL_API cl_object si_run_program _ECL_ARGS((cl_narg narg, cl_object command, cl_object args, ...));
extern ECL_API cl_object si_external_process_wait _ECL_ARGS((cl_narg narg, cl_object h, ...));
extern ECL_API cl_object si_close_windows_handle(cl_object h);


/* unicode -- no particular file, but we group these changes here */

#ifdef ECL_UNICODE
extern ECL_API cl_object si_base_char_p(cl_object x);
extern ECL_API cl_object si_base_string_p(cl_object x);
extern ECL_API cl_object si_coerce_to_base_string(cl_object x);
extern ECL_API cl_object si_coerce_to_extended_string(cl_object x);
#define ecl_alloc_simple_extended_string(l) ecl_alloc_simple_vector((l),ecl_aet_ch)
extern ECL_API cl_object ecl_alloc_adjustable_extended_string(cl_index l);
# ifdef ECL_UNICODE_NAMES
extern ECL_API cl_object _ecl_ucd_code_to_name(ecl_character c);
extern ECL_API cl_object _ecl_ucd_name_to_code(cl_object name);
# endif
#else
#define si_base_char_p cl_characterp
#define si_base_string_p cl_stringp
#define si_coerce_to_base_string cl_string
#define si_coerce_to_extended_string cl_string
#endif

/* vector_push.d  */

extern ECL_API ecl_character ecl_string_push_extend(cl_object s, ecl_character c);
extern ECL_API cl_object cl_vector_push _ECL_ARGS((cl_object V1, cl_object V2));
extern ECL_API cl_object cl_vector_push_extend _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));


/**********************************************************************
 * FUNCTIONS GENERATED BY THE LISP COMPILER
 */

/* arraylib.lsp */
extern ECL_API cl_object cl_make_array _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object si_fill_array_with_seq _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_vector _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_array_dimensions(cl_object V1);
extern ECL_API cl_object cl_array_in_bounds_p _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_array_row_major_index _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_bit _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_sbit _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_bit_and _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_ior _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_xor _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_eqv _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_nand _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_nor _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_andc1 _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_andc2 _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_orc1 _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_orc2 _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_not _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_vector_pop(cl_object V1);
extern ECL_API cl_object cl_adjust_array _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));

/* config.lsp */

extern ECL_API cl_object cl_short_site_name();
extern ECL_API cl_object cl_long_site_name();
extern ECL_API cl_object cl_lisp_implementation_type();
extern ECL_API cl_object cl_lisp_implementation_version();
extern ECL_API cl_object si_lisp_implementation_vcs_id();
extern ECL_API cl_object cl_machine_type();
extern ECL_API cl_object cl_machine_instance();
extern ECL_API cl_object cl_machine_version();
extern ECL_API cl_object cl_software_type();
extern ECL_API cl_object cl_software_version();

/* describe.lsp */

extern ECL_API cl_object cl_inspect(cl_object o);
extern ECL_API cl_object cl_describe _ECL_ARGS((cl_narg narg, cl_object o, ...));

/* iolib.lsp */

extern ECL_API cl_object cl_read_from_string _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_write_to_string _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_prin1_to_string(cl_object V1);
extern ECL_API cl_object cl_princ_to_string(cl_object V1);
extern ECL_API cl_object cl_y_or_n_p _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_yes_or_no_p _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_string_to_object _ECL_ARGS((cl_narg narg, cl_object str, ...));
extern ECL_API cl_object cl_dribble _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_load_encoding(cl_object name);
extern ECL_API cl_object si_make_encoding(cl_object mapping);

/* listlib.lsp */

extern ECL_API cl_object cl_union _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_nunion _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_intersection _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_nintersection _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_set_difference _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_nset_difference _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_set_exclusive_or _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_nset_exclusive_or _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_subsetp _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_rassoc_if _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_rassoc_if_not _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_assoc_if _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_assoc_if_not _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_member_if _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_member_if_not _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_subst_if _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_subst_if_not _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_nsubst_if _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_nsubst_if_not _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));

/* mislib.lsp */

extern ECL_API cl_object cl_logical_pathname_translations(cl_object V1);
extern ECL_API cl_object cl_load_logical_pathname_translations(cl_object V1);
extern ECL_API cl_object cl_decode_universal_time _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_encode_universal_time _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, cl_object V4, cl_object V5, cl_object V6, ...));
extern ECL_API cl_object cl_get_decoded_time();
extern ECL_API cl_object cl_ensure_directories_exist _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object si_simple_program_error _ECL_ARGS((cl_narg narg, cl_object format, ...)) ecl_attr_noreturn;
extern ECL_API cl_object si_signal_simple_error _ECL_ARGS((cl_narg narg, cl_object condition, cl_object continuable, cl_object format, cl_object args, ...));

/* module.lsp */

extern ECL_API cl_object cl_provide(cl_object V1);
extern ECL_API cl_object cl_require _ECL_ARGS((cl_narg narg, cl_object V1, ...));

/* numlib.lsp */

extern ECL_API cl_object cl_isqrt(cl_object V1);
extern ECL_API cl_object cl_phase(cl_object V1);
extern ECL_API cl_object cl_signum(cl_object V1);
extern ECL_API cl_object cl_cis(cl_object V1);
extern ECL_API cl_object cl_asin(cl_object V1);
extern ECL_API cl_object cl_acos(cl_object V1);
extern ECL_API cl_object cl_asinh(cl_object V1);
extern ECL_API cl_object cl_acosh(cl_object V1);
extern ECL_API cl_object cl_atanh(cl_object V1);
extern ECL_API cl_object cl_ffloor _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_fceiling _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_ftruncate _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_fround _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_logtest(cl_object V1, cl_object V2);
extern ECL_API cl_object cl_byte(cl_object V1, cl_object V2);
extern ECL_API cl_object cl_byte_size(cl_object V1);
extern ECL_API cl_object cl_byte_position(cl_object V1);
extern ECL_API cl_object cl_ldb(cl_object V1, cl_object V2);
extern ECL_API cl_object cl_ldb_test(cl_object V1, cl_object V2);
extern ECL_API cl_object cl_mask_field(cl_object V1, cl_object V2);
extern ECL_API cl_object cl_dpb(cl_object V1, cl_object V2, cl_object V3);
extern ECL_API cl_object cl_deposit_field(cl_object V1, cl_object V2, cl_object V3);

/* packlib.lsp */

extern ECL_API cl_object cl_find_all_symbols(cl_object V1);
extern ECL_API cl_object cl_apropos _ECL_ARGS((cl_narg arg, cl_object V1, ...));
extern ECL_API cl_object cl_apropos_list _ECL_ARGS((cl_narg arg, cl_object V1, ...));
extern ECL_API cl_object si_find_relative_package _ECL_ARGS((cl_narg narg, cl_object pack_name, ...));

/* predlib.lsp */

extern ECL_API cl_object si_subclassp _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object si_of_class_p _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object si_do_deftype _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_upgraded_array_element_type _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_upgraded_complex_part_type _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_typep _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_coerce(cl_object V1, cl_object V2);
extern ECL_API cl_object cl_subtypep _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object si_short_float_p(cl_object t);
extern ECL_API cl_object si_single_float_p(cl_object t);
extern ECL_API cl_object si_double_float_p(cl_object t);
extern ECL_API cl_object si_long_float_p(cl_object t);
extern ECL_API cl_object si_ratiop(cl_object t);

/* setf.lsp */

extern ECL_API cl_object si_do_defsetf(cl_object name, cl_object function);
extern ECL_API cl_object si_do_define_setf_method(cl_object name, cl_object function);

/* seq.lsp */

extern ECL_API cl_object cl_make_sequence _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_concatenate _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_map _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_some _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_every _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_notany _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_notevery _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_map_into _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));

/* seqlib.lsp */

extern ECL_API cl_object cl_reduce _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_fill _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_replace _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_remove _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_remove_if _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_remove_if_not _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_delete _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_delete_if _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_delete_if_not _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_count _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_count_if _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_count_if_not _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_substitute _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_substitute_if _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_substitute_if_not _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_nsubstitute _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_nsubstitute_if _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_nsubstitute_if_not _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_find _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_find_if _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_find_if_not _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_position _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_position_if _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_position_if_not _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_remove_duplicates _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_delete_duplicates _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_mismatch _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_search _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_sort _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_stable_sort _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_merge _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, cl_object V4, ...));
extern ECL_API cl_object cl_constantly(cl_object V1);
extern ECL_API cl_object cl_complement(cl_object V1);

/* trace.lsp */

extern ECL_API cl_object si_traced_old_definition(cl_object V1);

/* pprint.lsp */

extern ECL_API cl_object cl_pprint_newline _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_pprint_indent _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_pprint_tab _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_pprint_fill _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_pprint_linear _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_pprint_tabular _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_copy_pprint_dispatch _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_pprint_dispatch _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_set_pprint_dispatch _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));

#ifdef CLOS

/* combin.lsp */
extern ECL_API cl_object cl_method_combination_error _ECL_ARGS((cl_narg narg, cl_object format, ...));
extern ECL_API cl_object cl_invalid_method_error _ECL_ARGS((cl_narg narg, cl_object method, cl_object format, ...));
extern ECL_API cl_object clos_std_compute_applicable_methods(cl_object gf, cl_object arglist);
extern ECL_API cl_object clos_std_compute_effective_method(cl_object gf, cl_object combination, cl_object methods_list);
extern ECL_API cl_object clos_compute_effective_method_function(cl_object gf, cl_object combination, cl_object methods_list);

/* boot.lsp */
extern ECL_API cl_object cl_slot_boundp(cl_object object, cl_object slot);
extern ECL_API cl_object cl_slot_makunbound(cl_object object, cl_object slot);
extern ECL_API cl_object cl_slot_exists_p(cl_object object, cl_object slot);

/* print.lsp */
extern ECL_API cl_object clos_need_to_make_load_form_p(cl_object o, cl_object env);

/* defclass.lsp */
extern ECL_API cl_object clos_load_defclass(cl_object name, cl_object superclasses, cl_object slots, cl_object options);

#if 0
/* defclass.lsp */
extern ECL_API cl_object clos_ensure_class _ECL_ARGS((cl_narg narg, cl_object V1, ...));

/* kernel.lsp */
extern ECL_API cl_object clos_class_id _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_class_direct_superclasses _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_class_direct_subclasses _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_class_slots _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_class_precedence_list _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_class_direct_slots _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_slot_index_table _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_class_shared_slots _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_generic_function_method_combination _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_generic_function_lambda_list _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_generic_function_argument_precedence_order _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_generic_function_method_class _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_generic_function_methods _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_method_generic_function _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_method_lambda_list _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_method_specializers _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_method_qualifiers _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_method_function _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_method_plist _ECL_ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_install_method _ECL_ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, cl_object V4, cl_object V5, cl_object V6, cl_object V7, ...));

#endif

/* std-slot-value */
extern ECL_API cl_object cl_slot_value(cl_object object, cl_object slot);
extern ECL_API cl_object clos_slot_value_set(cl_object value, cl_object instance, cl_object name);
extern ECL_API cl_object clos_standard_instance_access(cl_object object, cl_object location);
#define clos_funcallable_standard_instance_access clos_standard_instance_access
extern ECL_API cl_object clos_standard_instance_set(cl_object object, cl_object location, cl_object value);
#define clos_funcallable_standard_instance_set clos_standard_instance_access

/* method.lsp */
extern ECL_API cl_object clos_extract_lambda_list(cl_object object);
extern ECL_API cl_object clos_extract_specializer_names(cl_object object);

#endif

/* conditions.lsp */
extern ECL_API cl_object cl_abort _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_continue _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_compute_restarts _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_find_restart _ECL_ARGS((cl_narg narg, cl_object name, ...));
extern ECL_API cl_object cl_invoke_restart _ECL_ARGS((cl_narg narg, cl_object restart, ...));
extern ECL_API cl_object cl_invoke_restart_interactively(cl_object restart);
extern ECL_API cl_object cl_make_condition _ECL_ARGS((cl_narg narg, cl_object type, ...));
extern ECL_API cl_object cl_muffle_warning _ECL_ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_store_value _ECL_ARGS((cl_narg narg, cl_object value, ...));
extern ECL_API cl_object cl_use_value _ECL_ARGS((cl_narg narg, cl_object value, ...));
extern ECL_API cl_object si_bind_simple_restarts(cl_object tag, cl_object names);
extern ECL_API cl_object si_bind_simple_handlers(cl_object tag, cl_object names);
extern ECL_API cl_object si_assert_failure _ECL_ARGS((cl_narg narg, cl_object V1, ...));

/* assert.lsp */
extern ECL_API cl_object si_wrong_type_argument _ECL_ARGS((cl_narg narg, cl_object value, cl_object type, ...));
extern ECL_API cl_object si_ccase_error(cl_object keyform, cl_object key, cl_object values);
extern ECL_API cl_object si_ecase_error(cl_object value, cl_object values) ecl_attr_noreturn;
extern ECL_API cl_object si_etypecase_error(cl_object value, cl_object type) ecl_attr_noreturn;
extern ECL_API cl_object si_ctypecase_error(cl_object keyplace, cl_object value, cl_object type);
extern ECL_API cl_object si_do_check_type(cl_object value, cl_object type, cl_object type_string, cl_object place);

/*
 * CDR-5
 */
extern ECL_API cl_object si_negative_fixnum_p(cl_object);
extern ECL_API cl_object si_non_negative_fixnum_p(cl_object);
extern ECL_API cl_object si_non_positive_fixnum_p(cl_object);
extern ECL_API cl_object si_positive_fixnum_p(cl_object);
extern ECL_API cl_object si_array_index_p(cl_object);

extern ECL_API cl_object si_negative_integer_p(cl_object);
extern ECL_API cl_object si_non_negative_integer_p(cl_object);
extern ECL_API cl_object si_non_positive_integer_p(cl_object);
extern ECL_API cl_object si_positive_integer_p(cl_object);

extern ECL_API cl_object si_negative_rational_p(cl_object);
extern ECL_API cl_object si_non_negative_rational_p(cl_object);
extern ECL_API cl_object si_non_positive_rational_p(cl_object);
extern ECL_API cl_object si_positive_rational_p(cl_object);

extern ECL_API cl_object si_negative_ratio_p(cl_object);
extern ECL_API cl_object si_non_negative_ratio_p(cl_object);
extern ECL_API cl_object si_non_positive_ratio_p(cl_object);
extern ECL_API cl_object si_positive_ratio_p(cl_object);

extern ECL_API cl_object si_negative_real_p(cl_object);
extern ECL_API cl_object si_non_negative_real_p(cl_object);
extern ECL_API cl_object si_non_positive_real_p(cl_object);
extern ECL_API cl_object si_positive_real_p(cl_object);

extern ECL_API cl_object si_negative_float_p(cl_object);
extern ECL_API cl_object si_non_negative_float_p(cl_object);
extern ECL_API cl_object si_non_positive_float_p(cl_object);
extern ECL_API cl_object si_positive_float_p(cl_object);

extern ECL_API cl_object si_negative_short_float_p(cl_object);
extern ECL_API cl_object si_non_negative_short_float_p(cl_object);
extern ECL_API cl_object si_non_positive_short_float_p(cl_object);
extern ECL_API cl_object si_positive_short_float_p(cl_object);

extern ECL_API cl_object si_negative_single_float_p(cl_object);
extern ECL_API cl_object si_non_negative_single_float_p(cl_object);
extern ECL_API cl_object si_non_positive_single_float_p(cl_object);
extern ECL_API cl_object si_positive_single_float_p(cl_object);

extern ECL_API cl_object si_negative_double_float_p(cl_object);
extern ECL_API cl_object si_non_negative_double_float_p(cl_object);
extern ECL_API cl_object si_non_positive_double_float_p(cl_object);
extern ECL_API cl_object si_positive_double_float_p(cl_object);

extern ECL_API cl_object si_negative_long_float_p(cl_object);
extern ECL_API cl_object si_non_negative_long_float_p(cl_object);
extern ECL_API cl_object si_non_positive_long_float_p(cl_object);
extern ECL_API cl_object si_positive_long_float_p(cl_object);

#ifdef __cplusplus
}
#endif

#endif /* !ECL_EXTERNAL_H */
