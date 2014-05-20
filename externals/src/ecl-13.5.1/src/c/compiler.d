/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    compiler.c -- Bytecode compiler
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/*  Remarks:

    [1] The virtual machine has a word size of 16 bits. Operands and arguments
    have this very size, so that for instance, a jump

		OP_JMP increment

    takes two words of memory: one for the operator and one for the argument.
    The interpreter is written with this assumption in mind, but it should be
    easily modifed, because arguments are retrieved with "next_arg" and
    operators with "next_op".  Parts which will require a careful modification
    are marked with flag [1].
*/
#include <string.h>
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/bytecodes.h>

/********************* EXPORTS *********************/

#define REGISTER_SPECIALS	1
#define IGNORE_DECLARATIONS	0

/* Flags for the compilation routines: */
/* + Push the output of this form */
#define FLAG_PUSH		1
/* + Set the output of this form in VALUES */
#define FLAG_VALUES		2
/* + Set the output of this form in REG0 */
#define FLAG_REG0		4
/* + Search function binding in the global environment */
#define FLAG_GLOBAL		8
/* + Ignore this form */
#define FLAG_IGNORE		0
#define FLAG_USEFUL		(FLAG_PUSH | FLAG_VALUES | FLAG_REG0)

#define FLAG_EXECUTE		16
#define FLAG_LOAD		32
#define FLAG_COMPILE		64
#define FLAG_ONLY_LOAD		128

#define ENV_RECORD_LOCATION(r)	CADDDR(r)

#define ECL_SPECIAL_VAR_REF	-2
#define ECL_UNDEFINED_VAR_REF	-1

/********************* PRIVATE ********************/

typedef struct cl_compiler_env *cl_compiler_ptr;

#define asm_begin(env) current_pc(env)
#define current_pc(env) ECL_STACK_INDEX(env)
#define set_pc(env,n) asm_clear(env,n)
#define asm_ref(env,n) (cl_fixnum)((env)->stack[n])
static void asm_clear(cl_env_ptr env, cl_index h);
static void asm_op(cl_env_ptr env, cl_fixnum op);
static void asm_op2(cl_env_ptr env, int op, int arg);
static cl_object asm_end(cl_env_ptr env, cl_index handle, cl_object definition);
static cl_index asm_jmp(cl_env_ptr env, register int op);
static void asm_complete(cl_env_ptr env, register int op, register cl_index original);

static cl_fixnum c_var_ref(cl_env_ptr env, cl_object var, int allow_symbol_macro, bool ensure_defined);

static int c_block(cl_env_ptr env, cl_object args, int flags);
static int c_case(cl_env_ptr env, cl_object args, int flags);
static int c_catch(cl_env_ptr env, cl_object args, int flags);
static int c_compiler_let(cl_env_ptr env, cl_object args, int flags);
static int c_cond(cl_env_ptr env, cl_object args, int flags);
static int c_eval_when(cl_env_ptr env, cl_object args, int flags);
static int c_flet(cl_env_ptr env, cl_object args, int flags);
static int c_funcall(cl_env_ptr env, cl_object args, int flags);
static int c_function(cl_env_ptr env, cl_object args, int flags);
static int c_go(cl_env_ptr env, cl_object args, int flags);
static int c_if(cl_env_ptr env, cl_object args, int flags);
static int c_labels(cl_env_ptr env, cl_object args, int flags);
static int c_let(cl_env_ptr env, cl_object args, int flags);
static int c_leta(cl_env_ptr env, cl_object args, int flags);
static int c_load_time_value(cl_env_ptr env, cl_object args, int flags);
static int c_locally(cl_env_ptr env, cl_object args, int flags);
static int c_macrolet(cl_env_ptr env, cl_object args, int flags);
static int c_multiple_value_bind(cl_env_ptr env, cl_object args, int flags);
static int c_multiple_value_call(cl_env_ptr env, cl_object args, int flags);
static int c_multiple_value_prog1(cl_env_ptr env, cl_object args, int flags);
static int c_multiple_value_setq(cl_env_ptr env, cl_object args, int flags);
static int c_not(cl_env_ptr env, cl_object args, int flags);
static int c_nth_value(cl_env_ptr env, cl_object args, int flags);
static int c_prog1(cl_env_ptr env, cl_object args, int flags);
static int c_progv(cl_env_ptr env, cl_object args, int flags);
static int c_psetq(cl_env_ptr env, cl_object args, int flags);
static int c_quote(cl_env_ptr env, cl_object args, int flags);
static int c_values(cl_env_ptr env, cl_object args, int flags);
static int c_setq(cl_env_ptr env, cl_object args, int flags);
static int c_return(cl_env_ptr env, cl_object args, int flags);
static int c_return_from(cl_env_ptr env, cl_object args, int flags);
static int c_symbol_macrolet(cl_env_ptr env, cl_object args, int flags);
static int c_tagbody(cl_env_ptr env, cl_object args, int flags);
static int c_the(cl_env_ptr env, cl_object args, int flags);
static int c_throw(cl_env_ptr env, cl_object args, int flags);
static int c_unwind_protect(cl_env_ptr env, cl_object args, int flags);
static int c_while(cl_env_ptr env, cl_object args, int flags);
static int c_with_backend(cl_env_ptr env, cl_object args, int flags);
static int c_until(cl_env_ptr env, cl_object args, int flags);
static void eval_form(cl_env_ptr env, cl_object form);
static int execute_each_form(cl_env_ptr env, cl_object body);
static int compile_toplevel_body(cl_env_ptr env, cl_object args, int flags);
static int compile_body(cl_env_ptr env, cl_object args, int flags);
static int compile_form(cl_env_ptr env, cl_object args, int push);
static int compile_with_load_time_forms(cl_env_ptr env, cl_object form, int flags);
static int compile_constant(cl_env_ptr env, cl_object stmt, int flags);

static int c_cons(cl_env_ptr env, cl_object args, int push);
static int c_endp(cl_env_ptr env, cl_object args, int push);
static int c_car(cl_env_ptr env, cl_object args, int push);
static int c_cdr(cl_env_ptr env, cl_object args, int push);
static int c_list(cl_env_ptr env, cl_object args, int push);
static int c_listA(cl_env_ptr env, cl_object args, int push);

static cl_object ecl_make_lambda(cl_env_ptr env, cl_object name, cl_object lambda);

static void FEillegal_variable_name(cl_object) ecl_attr_noreturn;
static void FEill_formed_input(void) ecl_attr_noreturn;

/* -------------------- SAFE LIST HANDLING -------------------- */

static cl_object
pop(cl_object *l) {
	cl_object head, list = *l;
	unlikely_if (ECL_ATOM(list))
		FEill_formed_input();
	head = ECL_CONS_CAR(list);
	*l = ECL_CONS_CDR(list);
	return head;
}

static cl_object
pop_maybe_nil(cl_object *l) {
	cl_object head, list = *l;
	if (list == ECL_NIL)
		return ECL_NIL;
	unlikely_if (!ECL_LISTP(list))
		FEill_formed_input();
	head = ECL_CONS_CAR(list);
	*l = ECL_CONS_CDR(list);
	return head;
}

/* ------------------------------ ASSEMBLER ------------------------------ */

static cl_object
asm_end(cl_env_ptr env, cl_index beginning, cl_object definition) {
        const cl_compiler_ptr c_env = env->c_env;
	cl_object bytecodes;
	cl_index code_size, i;
	cl_opcode *code;
        cl_object file = ECL_SYM_VAL(env,@'ext::*source-location*'), position;
        if (Null(file)) {
                file = ECL_SYM_VAL(env,@'*load-truename*');
                position = ecl_make_fixnum(0);
        } else {
                position = cl_cdr(file);
                file = cl_car(file);
        }

	/* Save bytecodes from this session in a new vector */
	code_size = current_pc(env) - beginning;
	bytecodes = ecl_alloc_object(t_bytecodes);
	bytecodes->bytecodes.name = @'si::bytecodes';
        bytecodes->bytecodes.definition = definition;
	bytecodes->bytecodes.code_size = code_size;
	bytecodes->bytecodes.code = ecl_alloc_atomic(code_size * sizeof(cl_opcode));
	bytecodes->bytecodes.data = c_env->constants;
	for (i = 0, code = (cl_opcode *)bytecodes->bytecodes.code; i < code_size; i++) {
		code[i] = (cl_opcode)(cl_fixnum)(env->stack[beginning+i]);
 	}
        bytecodes->bytecodes.entry =  _ecl_bytecodes_dispatch_vararg;
        ecl_set_function_source_file_info(bytecodes, (file == OBJNULL)? ECL_NIL : file,
                                          (file == OBJNULL)? ECL_NIL : position);
	asm_clear(env, beginning);
	return bytecodes;
}

#if defined(ECL_SMALL_BYTECODES)
static void
asm_arg(cl_env_ptr env, int n) {
#ifdef WORDS_BIGENDIAN
	asm_op(env, (n >> 8) & 0xFF);
	asm_op(env, n & 0xFF);
#else
	asm_op(env, n & 0xFF);
	asm_op(env, (n >> 8) & 0xFF);
#endif
}
#else
#define asm_arg(env,n) asm_op(env,n)
#endif

static void
asm_op(cl_env_ptr env, cl_fixnum code) {
        cl_object v = (cl_object)code;
        ECL_STACK_PUSH(env,v);
}

static void
asm_clear(cl_env_ptr env, cl_index h) {
        ECL_STACK_SET_INDEX(env, h);
}

static void
asm_op2(cl_env_ptr env, int code, int n) {
	if (ecl_unlikely(n < -MAX_OPARG || MAX_OPARG < n))
		FEprogram_error_noreturn("Argument to bytecode is too large", 0);
	asm_op(env, code);
	asm_arg(env, n);
}

static cl_index
asm_constant(cl_env_ptr env, cl_object c)
{
        const cl_compiler_ptr c_env = env->c_env;
	cl_object constants = c_env->constants;
	cl_vector_push_extend(2, c, constants);
	return constants->vector.fillp-1;
}

static cl_index
asm_jmp(cl_env_ptr env, int op) {
	cl_index output;
	asm_op(env, op);
	output = current_pc(env);
	asm_arg(env, 0);
	return output;
}

static void
asm_complete(cl_env_ptr env, int op, cl_index pc) {
	cl_fixnum delta = current_pc(env) - pc;  /* [1] */
	if (ecl_unlikely(op && (asm_ref(env, pc-1) != op)))
		FEprogram_error_noreturn("Non matching codes in ASM-COMPLETE2", 0);
	else if (ecl_unlikely(delta < -MAX_OPARG || delta > MAX_OPARG))
		FEprogram_error_noreturn("Too large jump", 0);
	else {
#ifdef ECL_SMALL_BYTECODES
		unsigned char low = delta & 0xFF;
		char high = delta >> 8;
# ifdef WORDS_BIGENDIAN
		env->stack[pc] = (cl_object)(cl_fixnum)high;
		env->stack[pc+1] = (cl_object)(cl_fixnum)low;
# else
		env->stack[pc] = (cl_object)(cl_fixnum)low;
		env->stack[pc+1] = (cl_object)(cl_fixnum)high;
# endif
#else
		env->stack[pc] = (cl_object)(cl_fixnum)delta;
#endif
	}
}

/* ------------------------------ COMPILER ------------------------------ */

typedef struct {
  void *symbol;
  int (*compiler)(cl_env_ptr, cl_object, int);
  int lexical_increment;
} compiler_record;

static compiler_record database[] = {
  {@'block', c_block, 1},
  {@'case', c_case, 1},
  {@'catch', c_catch, 1},
  {@'ext::compiler-let', c_compiler_let, 0},
  {@'cond', c_cond, 1},
  {@'eval-when', c_eval_when, 0},
  {@'flet', c_flet, 1},
  {@'function', c_function, 1},
  {@'funcall', c_funcall, 1},
  {@'go', c_go, 1},
  {@'if', c_if, 1},
  {@'labels', c_labels, 1},
  {@'let', c_let, 1},
  {@'let*', c_leta, 1},
  {@'locally', c_locally, 0},
  {@'load-time-value', c_load_time_value, 1},
  {@'macrolet', c_macrolet, 0},
  {@'multiple-value-bind', c_multiple_value_bind, 1},
  {@'multiple-value-call', c_multiple_value_call, 1},
  {@'multiple-value-prog1', c_multiple_value_prog1, 1},
  {@'multiple-value-setq', c_multiple_value_setq, 1},
  {@'not', c_not, 1},
  {@'nth-value', c_nth_value, 1},
  {@'null', c_not, 1},
  {@'progn', compile_toplevel_body, 0},
  {@'prog1', c_prog1, 1},
  {@'progv', c_progv, 1},
  {@'psetq', c_psetq, 1},
  {@'quote', c_quote, 1},
  {@'return', c_return, 1},
  {@'return-from', c_return_from, 1},
  {@'setq', c_setq, 1},
  {@'symbol-macrolet', c_symbol_macrolet, 0},
  {@'tagbody', c_tagbody, 1},
  {@'the', c_the, 0},
  {@'ext::truly-the', c_the, 0},
  {@'throw', c_throw, 1},
  {@'unwind-protect', c_unwind_protect, 1},
  {@'values', c_values, 1},
  {@'si::while', c_while, 0},
  {@'ext::with-backend', c_with_backend, 0},
  {@'si::until', c_until, 0},

  /* Extras */

  {@'cons', c_cons, 1},
  {@'car', c_car, 1},
  {@'cdr', c_cdr, 1},
  {@'first', c_car, 1},
  {@'rest', c_cdr, 1},
  {@'list', c_list, 1},
  {@'list*', c_listA, 1},
  {@'endp', c_endp, 1},
  {@'si::cons-car', c_car, 1},
  {@'si::cons-cdr', c_cdr, 1},
  {NULL, NULL, 1}
};

/* ----------------- LEXICAL ENVIRONMENT HANDLING -------------------- */

static void
assert_type_symbol(cl_object v)
{
	if (ecl_t_of(v) != t_symbol)
		FEprogram_error_noreturn("Expected a symbol, found ~S.", 1, v);
}

static void
FEillegal_variable_name(cl_object v)
{
	FEprogram_error_noreturn("Not a valid variable name ~S.", 1, v);
}

static void
FEill_formed_input()
{
	FEprogram_error_noreturn("Syntax error: list with too few elements or improperly terminated.", 0);
}

static int
c_search_constant(cl_env_ptr env, cl_object c)
{
        const cl_compiler_ptr c_env = env->c_env;
	cl_object p = c_env->constants;
	int n;
	for (n = 0; n < p->vector.fillp; n++) {
		if (ecl_eql(p->vector.self.t[n], c)) {
			return n;
		}
	}
	return -1;
}

static int
c_register_constant(cl_env_ptr env, cl_object c)
{
	int n = c_search_constant(env, c);
	return (n < 0)?
		asm_constant(env, c) :
		n;
}

static void
asm_c(cl_env_ptr env, cl_object o) {
	asm_arg(env, c_register_constant(env, o));
}

static void
asm_op2c(cl_env_ptr env, int code, cl_object o) {
	asm_op2(env, code, c_register_constant(env, o));
}

/*
 * Note: the following should match the definitions in cmp/cmpenv.lsp, as
 * well as CMP-ENV-REGISTER-MACROLET (lsp/defmacro.lsp)
 *
 * The compiler environment consists of two lists, one stored in
 * env->variables, the other one stored in env->macros.
 *
 * variable-record =	(:block block-name [used-p | block-object] location) |
 *			(:tag ({tag-name}*) [NIL | tag-object] location) |
 *			(:function function-name used-p [location]) |
 *			(var-name {:special | nil} bound-p [location]) |
 *			(symbol si::symbol-macro macro-function) |
 *			CB | LB | UNWIND-PROTECT |
 *			(:declare declaration-arguments*)
 * macro-record =	(function-name FUNCTION [| function-object]) |
 *			(macro-name si::macro macro-function)
 *			CB | LB | UNWIND-PROTECT
 *
 * A *-NAME is a symbol. A TAG-ID is either a symbol or a number. A
 * MACRO-FUNCTION is a function that provides us with the expansion
 * for that local macro or symbol macro. BOUND-P is true when the
 * variable has been bound by an enclosing form, while it is NIL if
 * the variable-record corresponds just to a special declaration.
 * CB, LB and UNWIND-PROTECT are only used by the C compiler and they
 * denote closure, lexical environment and unwind-protect boundaries.
 *
 * The brackets [] denote differences between the bytecodes and C
 * compiler environments, with the first option belonging to the
 * interpreter and the second alternative to the compiler.
 *
 * A LOCATION object is proper to the bytecodes compiler and denotes
 * the position of this variable, block, tag or function, in the
 * lexical environment. Currently, it is a CONS with two integers
 * (DEPTH . ORDER), denoting the depth of the nested environments and
 * the position in the environment (from the beginning, not from the
 * tail).
 *
 * The BLOCK-, TAG- and FUNCTION- objects are proper of the compiler
 * and carry further information.
 *
 * The last variable records are devoted to declarations and are only
 * used by the C compiler. Read cmpenv.lsp for more details on the
 * structure of these declaration forms, as they do not completely
 * match those of Common-Lisp.
 */

#if 0
#define new_location(env,x) ecl_make_fixnum(0)
#else
static cl_object
new_location(const cl_compiler_ptr c_env)
{
	return CONS(ecl_make_fixnum(c_env->env_depth),
                    ecl_make_fixnum(c_env->env_size++));
}
#endif

static cl_index
c_register_block(cl_env_ptr env, cl_object name)
{
        const cl_compiler_ptr c_env = env->c_env;
	cl_object loc = new_location(c_env);
	c_env->variables = CONS(cl_list(4, @':block', name, ECL_NIL, loc),
                                c_env->variables);
	return ecl_fixnum(ECL_CONS_CDR(loc));
}

static cl_index
c_register_tags(cl_env_ptr env, cl_object all_tags)
{
        const cl_compiler_ptr c_env = env->c_env;
	cl_object loc = new_location(c_env);
	c_env->variables = CONS(cl_list(4, @':tag', all_tags, ECL_NIL, loc),
                                c_env->variables);
	return ecl_fixnum(ECL_CONS_CDR(loc));
}

static void
c_register_function(cl_env_ptr env, cl_object name)
{
        const cl_compiler_ptr c_env = env->c_env;
	c_env->variables = CONS(cl_list(4, @':function', name, ECL_NIL,
                                        new_location(c_env)),
                                c_env->variables);
	c_env->macros = CONS(cl_list(2, name, @'function'), c_env->macros);
}

static cl_object
c_macro_expand1(cl_env_ptr env, cl_object stmt)
{
        const cl_compiler_ptr c_env = env->c_env;
	return cl_macroexpand_1(2, stmt, CONS(c_env->variables, c_env->macros));
}

static void
c_register_symbol_macro(cl_env_ptr env, cl_object name, cl_object exp_fun)
{
        const cl_compiler_ptr c_env = env->c_env;
	c_env->variables = CONS(cl_list(3, name, @'si::symbol-macro', exp_fun),
                                c_env->variables);
}

/* UNUSED
static void
c_register_macro(cl_env_ptr env, cl_object name, cl_object exp_fun)
{
        const cl_compiler_ptr c_env = env->c_env;
	c_env->macros = CONS(cl_list(3, name, @'si::macro', exp_fun), c_env->macros);
}
*/

static void
c_register_var(cl_env_ptr env, cl_object var, bool special, bool bound)
{
	const cl_compiler_ptr c_env = env->c_env;
	c_env->variables = CONS(cl_list(4, var,
					special? @'special' : ECL_NIL,
					bound? ECL_T : ECL_NIL,
					new_location(c_env)),
				c_env->variables);
}

static void
guess_environment(cl_env_ptr env, cl_object interpreter_env)
{
        if (!LISTP(interpreter_env))
                return;
	/*
	 * Given the environment of an interpreted function, we guess a
	 * suitable compiler enviroment to compile forms that access the
	 * variables and local functions of this interpreted code.
	 */
	for (interpreter_env = @revappend(interpreter_env, ECL_NIL);
	     !Null(interpreter_env);
	     interpreter_env = ECL_CONS_CDR(interpreter_env))
	{
		cl_object record = ECL_CONS_CAR(interpreter_env);
                if (!LISTP(record)) {
			c_register_function(env, record);
                } else {
                        cl_object record0 = ECL_CONS_CAR(record);
                        cl_object record1 = ECL_CONS_CDR(record);
                        if (ECL_SYMBOLP(record0)) {
                                c_register_var(env, record0, FALSE, TRUE);
                        } else if (record1 == ecl_make_fixnum(0)) {
                                c_register_tags(env, ECL_NIL);
                        } else {
                                c_register_block(env, record1);
                        }
                }
	}
}

static void
c_new_env(cl_env_ptr the_env, cl_compiler_env_ptr new, cl_object env,
          cl_compiler_env_ptr old)
{
	the_env->c_env = new;
	if (old) {
		*new = *old;
		new->env_depth = old->env_depth + 1;
	} else {
		new->code_walker = ECL_SYM_VAL(the_env, @'si::*code-walker*');
		new->constants = si_make_vector(ECL_T, ecl_make_fixnum(16),
						ECL_T, /* Adjustable */
						ecl_make_fixnum(0), /* Fillp */
						ECL_NIL, /* displacement */
						ECL_NIL);
		new->stepping = 0;
		new->lexical_level = 0;
		new->load_time_forms = ECL_NIL;
		new->env_depth = 0;
		new->macros = CDR(env);
		new->variables = CAR(env);
		for (env = new->variables; !Null(env); env = CDR(env)) {
			cl_object record = CAR(env);
			if (ECL_ATOM(record))
				continue;
			if (ECL_SYMBOLP(CAR(record)) && CADR(record) != @'si::symbol-macro') {
				continue;
			} else {
				new->lexical_level = 1;
				break;
			}
		}
                new->mode = FLAG_EXECUTE;
	}
	new->env_size = 0;
}

static cl_object
c_tag_ref(cl_env_ptr env, cl_object the_tag, cl_object the_type)
{
	cl_fixnum n = 0;
	cl_object l;
        const cl_compiler_ptr c_env = env->c_env;
	for (l = c_env->variables; CONSP(l); l = ECL_CONS_CDR(l)) {
		cl_object type, name, record = ECL_CONS_CAR(l);
		if (ECL_ATOM(record))
			continue;
		type = ECL_CONS_CAR(record);
                record = ECL_CONS_CDR(record);
		name = ECL_CONS_CAR(record);
		if (type == @':tag') {
			if (type == the_type) {
				cl_object label = ecl_assql(the_tag, name);
				if (!Null(label)) {
					return CONS(ecl_make_fixnum(n), ECL_CONS_CDR(label));
				}
			}
			n++;
		} else if (type == @':block' || type == @':function') {
			/* We compare with EQUAL, because of (SETF fname) */
			if (type == the_type && ecl_equal(name, the_tag)) {
				/* Mark as used */
                                record = ECL_CONS_CDR(record);
				ECL_RPLACA(record, ECL_T);
				return ecl_make_fixnum(n);
			}
			n++;
		} else if (Null(name)) {
			n++;
		} else {
			/* We are counting only locals and ignore specials
			 * and other declarations */
		}
	}
	return ECL_NIL;
}

ecl_def_ct_base_string(undefined_variable,
                       "Undefined variable referenced in interpreted code"
                       ".~%Name: ~A", 60, static, const);				

static cl_fixnum
c_var_ref(cl_env_ptr env, cl_object var, int allow_symbol_macro, bool ensure_defined)
{
	cl_fixnum n = 0;
	cl_object l, record, special, name;
        const cl_compiler_ptr c_env = env->c_env;
	for (l = c_env->variables; CONSP(l); l = ECL_CONS_CDR(l)) {
		record = ECL_CONS_CAR(l);
		if (ECL_ATOM(record))
			continue;
		name = ECL_CONS_CAR(record);
                record = ECL_CONS_CDR(record);
		special = ECL_CONS_CAR(record);
		if (name == @':block' || name == @':tag' || name == @':function') {
			n++;
		} else if (name == @':declare') {
			/* Ignored */
		} else if (name != var) {
			/* Symbol not yet found. Only count locals. */
			if (Null(special)) n++;
		} else if (special == @'si::symbol-macro') {
			/* We can only get here when we try to redefine a
			   symbol macro */
			if (allow_symbol_macro)
				return -1;
			FEprogram_error_noreturn("Internal error: symbol macro ~S"
                                                 " used as variable",
                                                 1, var);
		} else if (Null(special)) {
			return n;
		} else {
			return ECL_SPECIAL_VAR_REF;
		}
	}
	if (ensure_defined) {
		l = ecl_symbol_value(@'ext::*action-on-undefined-variable*');
		if (l != ECL_NIL) {
			funcall(3, l, undefined_variable, var);
		}
	}
	return ECL_UNDEFINED_VAR_REF;
}

static bool
c_declared_special(register cl_object var, register cl_object specials)
{
	return ((ecl_symbol_type(var) & ecl_stp_special) || ecl_member_eq(var, specials));
}

static void
c_declare_specials(cl_env_ptr env, cl_object specials)
{
	while (!Null(specials)) {
		int ndx;
		cl_object var = pop(&specials);
		ndx = c_var_ref(env, var, 1, FALSE);
		if (ndx >= 0 || ndx == ECL_UNDEFINED_VAR_REF)
			c_register_var(env, var, TRUE, FALSE);
	}
}

static cl_object
c_process_declarations(cl_object body)
{
	const cl_env_ptr the_env = ecl_process_env();
	@si::process-declarations(1, body);
	body = ecl_nth_value(the_env, 1);
	return body;
}

static bool
c_pbind(cl_env_ptr env, cl_object var, cl_object specials)
{
	bool special;
	if (!ECL_SYMBOLP(var))
		FEillegal_variable_name(var);
	else if ((special = c_declared_special(var, specials))) {
		c_register_var(env, var, TRUE, TRUE);
		asm_op2c(env, OP_PBINDS, var);
	} else {
		c_register_var(env, var, FALSE, TRUE);
		asm_op2c(env, OP_PBIND, var);
	}
	return special;
}

static bool
c_bind(cl_env_ptr env, cl_object var, cl_object specials)
{
	bool special;
	if (!ECL_SYMBOLP(var))
		FEillegal_variable_name(var);
	else if ((special = c_declared_special(var, specials))) {
		c_register_var(env, var, TRUE, TRUE);
		asm_op2c(env, OP_BINDS, var);
	} else {
		c_register_var(env, var, FALSE, TRUE);
		asm_op2c(env, OP_BIND, var);
	}
	return special;
}

static void
c_undo_bindings(cl_env_ptr the_env, cl_object old_vars, int only_specials)
{
	cl_object env;
	cl_index num_lexical = 0;
	cl_index num_special = 0;
        const cl_compiler_ptr c_env = the_env->c_env;

	for (env = c_env->variables; env != old_vars && !Null(env); env = ECL_CONS_CDR(env))
	{
                cl_object record, name, special;
                record = ECL_CONS_CAR(env);
		name = ECL_CONS_CAR(record);
                record = ECL_CONS_CDR(record);
		special = ECL_CONS_CAR(record);
		if (name == @':block' || name == @':tag') {
			(void)0;
		} else if (name == @':function' || Null(special)) {
			if (!only_specials) ++num_lexical;
		} else if (name == @':declare') {
			/* Ignored */
		} else if (special != @'si::symbol-macro') {
			/* If (third special) = NIL, the variable was declared
			   special, but there is no binding! */
                        record = ECL_CONS_CDR(record);
			if (!Null(ECL_CONS_CAR(record))) {
				num_special++;
			}
		}
	}
	c_env->variables = env;
	if (num_lexical) asm_op2(the_env, OP_UNBIND, num_lexical);
	if (num_special) asm_op2(the_env, OP_UNBINDS, num_special);
}

static void
compile_setq(cl_env_ptr env, int op, cl_object var)
{
	cl_fixnum ndx;

	if (!ECL_SYMBOLP(var))
		FEillegal_variable_name(var);
	ndx = c_var_ref(env, var,0,TRUE);
	if (ndx < 0) { /* Not a lexical variable */
		if (ecl_symbol_type(var) & ecl_stp_constant) {
			FEassignment_to_constant(var);
		}
		ndx = c_register_constant(env, var);
		if (op == OP_SETQ)
			op = OP_SETQS;
		else if (op == OP_PSETQ)
			op = OP_PSETQS;
		else if (op == OP_VSETQ)
			op = OP_VSETQS;
	}
	asm_op2(env, op, ndx);
}

/*
 * This routine is used to change the compilation flags in optimizers
 * that do not want to push values onto the stack.  Its purpose is to
 * keep ignorable forms ignored, while preserving the value of useful
 * forms. Qualitative behavior:
 *	FLAG_PUSH		-> FLAG_VALUES
 *	FLAG_VALUES		-> FLAG_VALUES
 *	FLAG_REG0		-> FLAG_REG0
 *	FLAG_IGNORE		-> FLAG_IGNORE
 */
static int
maybe_values_or_reg0(int flags) {
	if (flags & FLAG_PUSH)
		return (flags | FLAG_VALUES) & ~FLAG_PUSH;
	else
		return flags;
}

/*
 * This routine is used to change the compilation flags in optimizers
 * that do not want to push values onto the stack, but also do not want
 * to use REG0 (maybe because the call a nested ecl_interpret()). Ignorable
 * forms are kept ignored:
 *	FLAG_PUSH		-> FLAG_VALUES
 *	FLAG_VALUES		-> FLAG_VALUES
 *	FLAG_REG0		-> FLAG_VALUES
 *	FLAG_IGNORE		-> FLAG_IGNORE
 */
static int
maybe_values(int flags) {
	if (flags & FLAG_USEFUL)
		return (flags & ~(FLAG_PUSH | FLAG_REG0)) | FLAG_VALUES;
	else
		return flags;
}

/*
 * This routine is used to change the compilation flags in optimizers
 * that do not want to push values onto the stack.  Its purpose is to
 * keep ignorable forms ignored, while preserving the value of useful
 * forms. Qualitative behavior:
 *	FLAG_PUSH		-> FLAG_REG0
 *	FLAG_VALUES		-> FLAG_REG0
 *	FLAG_REG0		-> FLAG_REG0
 *	FLAG_IGNORE		-> FLAG_IGNORE
 */
static int
maybe_reg0(int flags) {
	if (flags & FLAG_USEFUL)
		return (flags & ~(FLAG_VALUES | FLAG_PUSH)) | FLAG_REG0;
	else
		return flags;
}

/* -------------------- THE COMPILER -------------------- */

/*
	The OP_BLOCK operator encloses several forms within a block
	named BLOCK_NAME, thus catching any OP_RETFROM whose argument
	matches BLOCK_NAME. The end of this block is marked both by
	the OP_EXIT operator and the LABELZ which is packed within
	the OP_BLOCK operator.

		[OP_BLOCK + name + labelz]
		....
		OP_EXIT_FRAME
	labelz:	...
*/

static int
c_block(cl_env_ptr env, cl_object body, int old_flags) {
	struct cl_compiler_env old_env;
	cl_object name = pop(&body);
	cl_object block_record;
	cl_index labelz, pc, loc, constants;
	int flags;

	if (!ECL_SYMBOLP(name))
		FEprogram_error_noreturn("BLOCK: Not a valid block name, ~S", 1, name);

	old_env = *(env->c_env);
	constants = old_env.constants->vector.fillp;
	pc = current_pc(env);

	flags = maybe_values_or_reg0(old_flags);
	loc = c_register_block(env, name);
	block_record = ECL_CONS_CAR(env->c_env->variables);
	if (Null(name)) {
		asm_op(env, OP_DO);
	} else {
		asm_op2c(env, OP_BLOCK, name);
	}
	labelz = asm_jmp(env, OP_FRAME);
	compile_body(env, body, flags);
	if (CADDR(block_record) == ECL_NIL) {
		/* Block unused. We remove the enclosing OP_BLOCK/OP_DO */
		/* We also have to remove the constants we compiled, because */
		/* some of them might be from load-time-value */
		old_env.constants->vector.fillp = constants;
		*(env->c_env) = old_env;
		set_pc(env, pc);
		return compile_body(env, body, old_flags);
	} else {
		c_undo_bindings(env, old_env.variables, 0);
		asm_op(env, OP_EXIT_FRAME);
		asm_complete(env, 0, labelz);
		return flags;
	}
}

/*
	There are several ways to invoke functions and to handle the
	output arguments. These are

		[OP_CALL + nargs]
		function_name

		[OP_FCALL + nargs]

	 OP_CALL and OP_FCALL leave all arguments in the VALUES() array,
	 while OP_PCALL and OP_PFCALL leave the first argument in the
	 stack.

	 OP_CALL and OP_PCALL use the value in VALUES(0) to retrieve the
	 function, while OP_FCALL and OP_PFCALL use a value from the
	 stack.
 */
static int
c_arguments(cl_env_ptr env, cl_object args) {
	cl_index nargs;
	for (nargs = 0; !Null(args); nargs++) {
		compile_form(env, pop(&args), FLAG_PUSH);
	}
	return nargs;
}

static int asm_function(cl_env_ptr env, cl_object args, int flags);

static int
c_call(cl_env_ptr env, cl_object args, int flags) {
	cl_object name;
	cl_index nargs;

	name = pop(&args);
	if (name >= (cl_object)cl_symbols
	    && name < (cl_object)(cl_symbols + cl_num_symbols_in_core))
	{
		cl_object f = ECL_SYM_FUN(name);
		cl_type t = (f == OBJNULL)? t_other : ecl_t_of(f);
		if (t == t_cfunfixed) {
			cl_index n = ecl_length(args);
			if (f->cfun.narg == 1 && n == 1) {
				compile_form(env, ECL_CONS_CAR(args), FLAG_REG0);
				asm_op2c(env, OP_CALLG1, name);
				return FLAG_VALUES;
			} else if (f->cfun.narg == 2 && n == 2) {
				compile_form(env, ECL_CONS_CAR(args), FLAG_PUSH);
				args = ECL_CONS_CDR(args);
				compile_form(env, ECL_CONS_CAR(args), FLAG_REG0);
				asm_op2c(env, OP_CALLG2, name);
				return FLAG_VALUES;
			}
		}
	}
	nargs = c_arguments(env, args);
	if (env->c_env->stepping) {
		/* When stepping, we only have one opcode to do function
		 * calls: OP_STEPFCALL. */
		asm_function(env, name, (flags & FLAG_GLOBAL) | FLAG_REG0);
		asm_op2(env, OP_STEPCALL, nargs);
		flags = FLAG_VALUES;
	} else if (ECL_SYMBOLP(name) &&
		   ((flags & FLAG_GLOBAL) || Null(c_tag_ref(env, name, @':function'))))
	{
		asm_op2(env, OP_CALLG, nargs);
		asm_c(env, name);
		flags = FLAG_VALUES;
	} else {
		/* Fixme!! We can optimize the case of global functions! */
		asm_function(env, name, (flags & FLAG_GLOBAL) | FLAG_REG0);
		asm_op2(env, OP_CALL, nargs);
		flags = FLAG_VALUES;
	}
	return flags;
}

static int
c_funcall(cl_env_ptr env, cl_object args, int flags) {
	cl_object name;
	cl_index nargs;

	name = pop(&args);
	if (CONSP(name)) {
                cl_object kind = ECL_CONS_CAR(name);
		if (kind == @'function') {
			if (cl_list_length(name) != ecl_make_fixnum(2))
				FEprogram_error_noreturn("FUNCALL: Invalid function name ~S",
                                                         1, name);
			return c_call(env, CONS(CADR(name), args), flags);
		}
		if (kind == @'quote') {
			if (cl_list_length(name) != ecl_make_fixnum(2))
				FEprogram_error_noreturn("FUNCALL: Invalid function name ~S",
                                                         1, name);
			return c_call(env, CONS(CADR(name), args), flags | FLAG_GLOBAL);
		}
	}
	compile_form(env, name, FLAG_PUSH);
	nargs = c_arguments(env, args);
	if (env->c_env->stepping) {
		asm_op2(env, OP_STEPCALL, nargs);
		flags = FLAG_VALUES;
	} else {
		asm_op2(env, OP_FCALL, nargs);
		flags = FLAG_VALUES;
	}
	asm_op(env, OP_POP1);
	return flags;
}

static int
perform_c_case(cl_env_ptr env, cl_object args, int flags) {
	cl_object test, clause;

	do {
		if (Null(args))
			return compile_body(env, ECL_NIL, flags);
		clause = pop(&args);
		if (ECL_ATOM(clause))
			FEprogram_error_noreturn("CASE: Illegal clause ~S.",1,clause);
		test = pop(&clause);
	} while (test == ECL_NIL);

	if (@'otherwise' == test || test == ECL_T) {
		unlikely_if (args != ECL_NIL) {
			FEprogram_error_noreturn("CASE: The selector ~A can only appear at the last position.",
						 1, test);
		}
		compile_body(env, clause, flags);
	} else {
		cl_index labeln, labelz;
		if (CONSP(test)) {
			cl_index n = ecl_length(test);
			while (n-- > 1) {
				cl_object v = pop(&test);
				asm_op(env, OP_JEQL);
				asm_c(env, v);
				asm_arg(env, n * (OPCODE_SIZE + OPARG_SIZE * 2)
					+ OPARG_SIZE);
			}
			test = ECL_CONS_CAR(test);
		}
		asm_op(env, OP_JNEQL);
		asm_c(env, test);
		labeln = current_pc(env);
		asm_arg(env, 0);
		compile_body(env, clause, flags);
		if (Null(args) && !(flags & FLAG_USEFUL)) {
			/* Ther is no otherwise. The test has failed and
			   we need no output value. We simply close jumps. */
			asm_complete(env, 0 & OP_JNEQL, labeln);
		} else {
			labelz = asm_jmp(env, OP_JMP);
			asm_complete(env, 0 & OP_JNEQL, labeln);
			perform_c_case(env, args, flags);
			asm_complete(env, OP_JMP, labelz);
		}
	}
	return flags;
}

static int
c_case(cl_env_ptr env, cl_object clause, int flags) {
	compile_form(env, pop(&clause), FLAG_REG0);
	return perform_c_case(env, clause, maybe_values_or_reg0(flags));
}

/*
	The OP_CATCH takes the object in VALUES(0) and uses it to catch
	any OP_THROW operation which uses that value as argument. If a
	catch occurs, or when all forms have been properly executed, it
	jumps to LABELZ. LABELZ is packed within the OP_CATCH operator.
		[OP_CATCH + labelz]
		...
		"forms to be caught"
		...
	       	OP_EXIT_FRAME
	labelz:	...
*/

static int
c_catch(cl_env_ptr env, cl_object args, int flags) {
	cl_index labelz, loc;
	cl_object old_env;

	/* Compile evaluation of tag */
	compile_form(env, pop(&args), FLAG_REG0);

	/* Compile binding of tag */
	old_env = env->c_env->variables;
	loc = c_register_block(env, ecl_make_fixnum(0));
	asm_op(env, OP_CATCH);

	/* Compile jump point */
	labelz = asm_jmp(env, OP_FRAME);

	/* Compile body of CATCH */
	compile_body(env, args, FLAG_VALUES);

	c_undo_bindings(env, old_env, 0);
	asm_op(env, OP_EXIT_FRAME);
	asm_complete(env, 0, labelz);

	return FLAG_VALUES;
}

static int
c_compiler_let(cl_env_ptr env, cl_object args, int flags) {
	cl_object bindings;
	cl_index old_bds_top_index = env->bds_top - env->bds_org;

	for (bindings = pop(&args); !Null(bindings); ) {
		cl_object form = pop(&bindings);
		cl_object var = pop(&form);
		cl_object value = pop_maybe_nil(&form);
		ecl_bds_bind(env, var, value);
	}
	flags = compile_toplevel_body(env, args, flags);
	ecl_bds_unwind(env, old_bds_top_index);
	return flags;
}

/*
	There are three operators which perform explicit jumps, but
	almost all other operators use labels in one way or
	another.

	1) Jumps are always relative to the place where the jump label
	is retrieved so that if the label is in vector[0], then the
	destination is roughly vector + vector[0].

	2) The three jump forms are

		[OP_JMP + label]	; Unconditional jump
		[OP_JNIL + label]	; Jump if VALUES(0) == ECL_NIL
		[OP_JT + label]		; Jump if VALUES(0) != ECL_NIL

	It is important to remark that both OP_JNIL and OP_JT truncate
	the values stack, so that always NVALUES = 1 after performing
	any of these operations.
*/
static int
c_cond(cl_env_ptr env, cl_object args, int flags) {
	cl_object test, clause;
	cl_index label_nil, label_exit;

	if (Null(args))
		return compile_form(env, ECL_NIL, flags);
	clause = pop(&args);
	if (ECL_ATOM(clause))
		FEprogram_error_noreturn("COND: Illegal clause ~S.",1,clause);
	test = pop(&clause);
	flags = maybe_values_or_reg0(flags);
	if (ECL_T == test) {
		/* Default sentence. If no forms, just output T. */
		if (Null(clause))
			compile_form(env, ECL_T, flags);
		else
			compile_body(env, clause, flags);
	} else {
		/* Compile the test. If no more forms, just output
		   the first value (this is guaranteed by OP_JT), but make
		   sure it is stored in the appropriate place. */
		if (Null(args)) {
			if (Null(clause)) {
				c_values(env, cl_list(1,test), flags);
			} else {
				compile_form(env, test, FLAG_REG0);
				if (flags & FLAG_VALUES) asm_op(env, OP_VALUEREG0);
				label_nil = asm_jmp(env, OP_JNIL);
				compile_body(env, clause, flags);
				asm_complete(env, OP_JNIL, label_nil);
			}
		} else if (Null(clause)) {
			compile_form(env, test, FLAG_REG0);
			if (flags & FLAG_VALUES) asm_op(env, OP_VALUEREG0);
			label_exit = asm_jmp(env, OP_JT);
			c_cond(env, args, flags);
			asm_complete(env, OP_JT, label_exit);
		} else {
			compile_form(env, test, FLAG_REG0);
			label_nil = asm_jmp(env, OP_JNIL);
			compile_body(env, clause, flags);
			label_exit = asm_jmp(env, OP_JMP);
			asm_complete(env, OP_JNIL, label_nil);
			c_cond(env, args, flags);
			asm_complete(env, OP_JMP, label_exit);
		}
	}
	return flags;
}

/*	The OP_DO operator saves the lexical environment and establishes
	a NIL block to execute the enclosed forms, which are typically
	like the ones shown below. At the exit of the block, either by
	means of a OP_RETFROM jump or because of normal termination,
	the lexical environment is restored, and all bindings undone.

		[OP_DO + labelz]
		...	; bindings
		[JMP + labelt]
	labelb:	...	; body
		...	; stepping forms
	labelt:	...	; test form
		[JNIL + label]
		...	; output form
		OP_EXIT_FRAME
	labelz:

*/
static int
c_while_until(cl_env_ptr env, cl_object body, int flags, bool is_while) {
	cl_object test = pop(&body);
	cl_index labelt, labelb;

	flags = maybe_reg0(flags);

	/* Jump to test */
	labelt = asm_jmp(env, OP_JMP);

	/* Compile body */
	labelb = current_pc(env);
	c_tagbody(env, body, flags);

	/* Compile test */
	asm_complete(env, OP_JMP, labelt);
	compile_form(env, test, FLAG_REG0);
	asm_op(env, is_while? OP_JT : OP_JNIL);
	asm_arg(env, labelb - current_pc(env));

	return flags;
}

static int
c_while(cl_env_ptr env, cl_object body, int flags) {
	return c_while_until(env, body, flags, 1);
}

static int
c_until(cl_env_ptr env, cl_object body, int flags) {
	return c_while_until(env, body, flags, 0);
}

static int
c_with_backend(cl_env_ptr env, cl_object args, int flags)
{
        cl_object forms = ECL_NIL;
        while (!Null(args)) {
                cl_object tag = pop(&args);
                cl_object form = pop(&args);
                if (tag == @':bytecodes')
                        forms = CONS(form, forms);
        }
        return compile_toplevel_body(env, forms, flags);
}

static int
eval_when_flags(cl_object situation)
{
        int code = 0;
        cl_object p;
        for (p = situation; p != ECL_NIL; p = ECL_CONS_CDR(p)) {
                cl_object keyword;
                unlikely_if (!ECL_LISTP(p))
                        FEtype_error_proper_list(situation);
                keyword = ECL_CONS_CAR(p);
                if (keyword == @'load')
                        code |= FLAG_LOAD;
                else if (keyword == @':load-toplevel')
                        code |= FLAG_LOAD;
                else if (keyword == @'compile')
                        code |= FLAG_COMPILE;
                else if (keyword == @':compile-toplevel')
                        code |= FLAG_COMPILE;
                else if (keyword == @'eval')
                        code |= FLAG_EXECUTE;
                else if (keyword == @':execute')
                        code |= FLAG_EXECUTE;
        }
        return code;
}

#define when_load_p(s) ((s) & FLAG_LOAD)
#define when_compile_p(s) ((s) & FLAG_COMPILE)
#define when_execute_p(s) ((s) & FLAG_EXECUTE)

static int
c_eval_when(cl_env_ptr env, cl_object args, int flags) {
        cl_object situation_list = pop(&args);
	int situation = eval_when_flags(situation_list);
        const cl_compiler_ptr c_env = env->c_env;
	int mode = c_env->mode;
        if (mode == FLAG_EXECUTE) {
                if (!when_execute_p(situation))
                        args = ECL_NIL;
        } else if (c_env->lexical_level) {
                if (!when_execute_p(situation))
                        args = ECL_NIL;
        } else if (mode == FLAG_LOAD) {
                if (when_compile_p(situation)) {
                        env->c_env->mode = FLAG_COMPILE;
			execute_each_form(env, args);
                        env->c_env->mode = FLAG_LOAD;
                        if (!when_load_p(situation))
                                args = ECL_NIL;
                } else if (when_load_p(situation)) {
                        env->c_env->mode = FLAG_ONLY_LOAD;
                        flags = compile_toplevel_body(env, args, flags);
                        env->c_env->mode = FLAG_LOAD;
                        return flags;
                } else {
                        args = ECL_NIL;
                }
        } else if (mode == FLAG_ONLY_LOAD) {
                if (!when_load_p(situation))
                        args = ECL_NIL;
        } else { /* FLAG_COMPILE */
                if (when_execute_p(situation) || when_compile_p(situation)) {
			execute_each_form(env, args);
                }
		args = ECL_NIL;
        }
        return compile_toplevel_body(env, args, flags);
}


/*
	The OP_FLET/OP_FLABELS operators change the lexical environment
	to add a few local functions.

		[OP_FLET/OP_FLABELS + nfun + fun1]
		...
		OP_UNBIND nfun
	labelz:
*/
static cl_index
c_register_functions(cl_env_ptr env, cl_object l)
{
	cl_index nfun;
	for (nfun = 0; !Null(l); nfun++) {
		cl_object definition = pop(&l);
		cl_object name = pop(&definition);
		c_register_function(env, name);
	}
	return nfun;
}

static int
c_labels_flet(cl_env_ptr env, int op, cl_object args, int flags) {
	cl_object l, def_list = pop(&args);
	cl_object old_vars = env->c_env->variables;
	cl_object old_funs = env->c_env->macros;
	cl_index nfun;

	if (ecl_length(def_list) == 0) {
		return c_locally(env, args, flags);
	}

	/* If compiling a LABELS form, add the function names to the lexical
	   environment before compiling the functions */
	if (op == OP_FLET)
		nfun = ecl_length(def_list);
	else
		nfun = c_register_functions(env, def_list);

	/* Push the operator (OP_LABELS/OP_FLET) with the number of functions */
	asm_op2(env, op, nfun);

	/* Compile the local functions now. */
	for (l = def_list; !Null(l); ) {
		cl_object definition = pop(&l);
		cl_object name = pop(&definition);
		cl_object lambda = ecl_make_lambda(env, name, definition);
		cl_index c = c_register_constant(env, lambda);
		asm_arg(env, c);
	}

	/* If compiling a FLET form, add the function names to the lexical
	   environment after compiling the functions */
	if (op == OP_FLET)
		c_register_functions(env, def_list);

	/* Compile the body of the form with the local functions in the lexical
	   environment. */
	flags = c_locally(env, args, flags);

	/* Restore and return */
	c_undo_bindings(env, old_vars, 0);
	env->c_env->macros = old_funs;

	return flags;
}


static int
c_flet(cl_env_ptr env, cl_object args, int flags) {
	return c_labels_flet(env, OP_FLET, args, flags);
}


/*
	There are two operators that produce functions. The first one
	is
		[OP_FUNCTION + name]
	which takes the function binding of SYMBOL. The second one is
		OP_CLOSE
		interpreted
	which encloses the INTERPRETED function in the current lexical
	environment.
*/
static int
c_function(cl_env_ptr env, cl_object args, int flags) {
	cl_object function = pop(&args);
	if (!Null(args))
		FEprogram_error_noreturn("FUNCTION: Too many arguments.", 0);
	return asm_function(env, function, flags);
}

static int
asm_function(cl_env_ptr env, cl_object function, int flags) {
	if (!Null(si_valid_function_name_p(function))) {
		cl_object ndx = c_tag_ref(env, function, @':function');
		if (Null(ndx)) {
			/* Globally defined function */
			asm_op2c(env, OP_FUNCTION, function);
                        return FLAG_REG0;
		} else {
			/* Function from a FLET/LABELS form */
			asm_op2(env, OP_LFUNCTION, ecl_fixnum(ndx));
                        return FLAG_REG0;
		}
	}
        if (CONSP(function)) {
                cl_object kind = ECL_CONS_CAR(function);
                cl_object body = ECL_CONS_CDR(function);
		cl_object name;
                if (kind == @'lambda') {
			name = ECL_NIL;
                } else if (kind == @'ext::lambda-block') {
                        name = ECL_CONS_CAR(body);
                        body = ECL_CONS_CDR(body);
                } else {
			goto ERROR;
		}
		{
			const cl_compiler_ptr c_env = env->c_env;
			asm_op2c(env,
				 (Null(c_env->variables) &&
				  Null(c_env->macros))?
				 OP_QUOTE : OP_CLOSE,
				 ecl_make_lambda(env, name, body));
		}
		return FLAG_REG0;
        }
 ERROR:
        FEprogram_error_noreturn("FUNCTION: Not a valid argument ~S.", 1, function);
	return FLAG_REG0;
}


static int
c_go(cl_env_ptr env, cl_object args, int flags) {
	cl_object tag = pop(&args);
	cl_object info = c_tag_ref(env, tag, @':tag');
	if (Null(info))
		FEprogram_error_noreturn("GO: Unknown tag ~S.", 1, tag);
	if (!Null(args))
		FEprogram_error_noreturn("GO: Too many arguments.",0);
	asm_op2(env, OP_GO, ecl_fixnum(CAR(info)));
	asm_arg(env, ecl_fixnum(CDR(info)));
	return flags;
}


/*
	(if a b) -> (cond (a b))
	(if a b c) -> (cond (a b) (t c))
*/
static int
c_if(cl_env_ptr env, cl_object form, int flags) {
	cl_object test = pop(&form);
	cl_object then = pop(&form);
	then = cl_list(2, test, then);
	if (Null(form)) {
		return c_cond(env, ecl_list1(then), flags);
	} else {
		return c_cond(env, cl_list(2, then, CONS(ECL_T, form)), flags);
	}
}


static int
c_labels(cl_env_ptr env, cl_object args, int flags) {
	return c_labels_flet(env, OP_LABELS, args, flags);
}


/*
	The OP_PUSHENV saves the current lexical environment to allow
	several bindings.
		OP_PUSHENV
		...		; binding forms
		...		; body
		OP_EXIT

	There are four forms which perform bindings
		OP_PBIND name	; Bind NAME in the lexical env. using
				; a value from the stack
		OP_PBINDS name	; Bind NAME as special variable using
				; a value from the stack
		OP_BIND name	; Bind NAME in the lexical env. using
				; VALUES(0)
		OP_BINDS name	; Bind NAME as special variable using
				; VALUES(0)

	After a variable has been bound, there are several ways to
	refer to it.

	1) Refer to the n-th variable in the lexical environment
		[SYMVAL + n]

	2) Refer to the value of a special variable or constant
		SYMVALS
		name

        3) Push the value of the n-th variable of the lexical environment
		[PUSHV + n]

	4) Push the value of a special variable or constant
		PUSHVS
		name
*/

static int
c_let_leta(cl_env_ptr env, int op, cl_object args, int flags) {
	cl_object bindings, specials, body, l, vars;
	cl_object old_variables = env->c_env->variables;

	bindings = cl_car(args);
	body = c_process_declarations(ECL_CONS_CDR(args));
	specials = env->values[3];

	/* Optimize some common cases */
	switch(ecl_length(bindings)) {
	case 0:		return c_locally(env, CDR(args), flags);
	case 1:		op = OP_BIND; break;
	}

	for (vars=ECL_NIL, l=bindings; !Null(l); ) {
		cl_object aux = pop(&l);
		cl_object var, value;
		if (ECL_ATOM(aux)) {
			var = aux;
			value = ECL_NIL;
		} else {
			var = pop(&aux);
			value = pop_maybe_nil(&aux);
			if (!Null(aux))
				FEprogram_error_noreturn("LET: Ill formed declaration.",0);
		}
		if (!ECL_SYMBOLP(var))
			FEillegal_variable_name(var);
		if (op == OP_PBIND) {
			compile_form(env, value, FLAG_PUSH);
			vars = CONS(var, vars);
		} else {
			compile_form(env, value, FLAG_REG0);
			c_bind(env, var, specials);
		}
	}
	while (!Null(vars))
		c_pbind(env, pop(&vars), specials);

	/* We have to register all specials, because in the list
	 * there might be some variable that is not bound by this LET form
	 */
	c_declare_specials(env, specials);

	flags = compile_body(env, body, flags);

	c_undo_bindings(env, old_variables, 0);
	return flags;
}

static int
c_let(cl_env_ptr env, cl_object args, int flags) {
	return c_let_leta(env, OP_PBIND, args, flags);
}

static int
c_leta(cl_env_ptr env, cl_object args, int flags) {
	return c_let_leta(env, OP_BIND, args, flags);
}

static int
c_load_time_value(cl_env_ptr env, cl_object args, int flags)
{
	const cl_compiler_ptr c_env = env->c_env;
	cl_object value;
	unlikely_if (Null(args) || cl_cddr(args) != ECL_NIL)
		FEprogram_error_noreturn("LOAD-TIME-VALUE: Wrong number of arguments.", 0);
	value = ECL_CONS_CAR(args);
        if (c_env->mode != FLAG_LOAD && c_env->mode != FLAG_ONLY_LOAD) {
		value = si_eval_with_env(1, value);
	} else if (ECL_SYMBOLP(value) || ECL_LISTP(value)) {
		/* Using the form as constant, we force the system to coalesce multiple
		 * copies of the same load-time-value form */
		c_env->load_time_forms =
			ecl_cons(cl_list(3, args, value, ECL_NIL),
				 c_env->load_time_forms);
		value = args;
	}
	return compile_constant(env, value, flags);
}

static int
c_locally(cl_env_ptr env, cl_object args, int flags) {
	cl_object old_env = env->c_env->variables;

	/* First use declarations by declaring special variables... */
	args = c_process_declarations(args);
	c_declare_specials(env, env->values[3]);

	/* ...and then process body */
	flags = compile_toplevel_body(env, args, flags);

	c_undo_bindings(env, old_env, 0);

	return flags;
}

/*
	MACROLET

	The current lexical environment is saved. A new one is prepared with
	the definitions of these macros, and this environment is used to
	compile the body.
 */
static int
c_macrolet(cl_env_ptr the_env, cl_object args, int flags)
{
        const cl_compiler_ptr c_env = the_env->c_env;
	cl_object old_env = c_env->macros;
	cl_object env = funcall(3, @'si::cmp-env-register-macrolet', pop(&args),
				CONS(c_env->variables, c_env->macros));
	c_env->macros = CDR(env);
	flags = c_locally(the_env, args, flags);
	c_env->macros = old_env;
	return flags;
}

static void
c_vbind(cl_env_ptr env, cl_object var, int n, cl_object specials)
{
        if (c_declared_special(var, specials)) {
                c_register_var(env, var, FLAG_PUSH, TRUE);
                if (n) {
                        asm_op2(env, OP_VBINDS, n);
                } else {
                        asm_op(env, OP_BINDS);
                }
        } else {
                c_register_var(env, var, FALSE, TRUE);
                if (n) {
                        asm_op2(env, OP_VBIND, n);
                } else {
                        asm_op(env, OP_BIND);
                }
        }
        asm_c(env, var);
}

static int
c_multiple_value_bind(cl_env_ptr env, cl_object args, int flags)
{
	cl_object vars = pop(&args);
	cl_object value = pop(&args);
        int n = ecl_length(vars);
	switch (n) {
        case 0:
                return c_locally(env, args, flags);
        case 1:
                vars = ECL_CONS_CAR(vars);
                vars = ecl_list1(cl_list(2, vars, value));
                return c_leta(env, cl_listX(2, vars, args), flags);
        default: {
                cl_object old_variables = env->c_env->variables;
                cl_object body = c_process_declarations(args);
                cl_object specials = env->values[3];
                compile_form(env, value, FLAG_VALUES);
		for (vars=cl_reverse(vars); n--; ) {
			cl_object var = pop(&vars);
			if (!ECL_SYMBOLP(var))
				FEillegal_variable_name(var);
                        c_vbind(env, var, n, specials);
		}
		c_declare_specials(env, specials);
		flags = compile_body(env, body, flags);
		c_undo_bindings(env, old_variables, 0);
                return flags;
	}}
}


static int
c_multiple_value_call(cl_env_ptr env, cl_object args, int flags) {
	cl_object name;
	int op;

	name = pop(&args);
	if (Null(args)) {
		/* If no arguments, just use ordinary call */
		return c_funcall(env, cl_list(1, name), flags);
	}
	compile_form(env, name, FLAG_PUSH);
	for (op = OP_PUSHVALUES; !Null(args); op = OP_PUSHMOREVALUES) {
		compile_form(env, pop(&args), FLAG_VALUES);
		asm_op(env, op);
	}
	asm_op(env, OP_MCALL);
	asm_op(env, OP_POP1);

	return FLAG_VALUES;
}


static int
c_multiple_value_prog1(cl_env_ptr env, cl_object args, int flags) {
	compile_form(env, pop(&args), FLAG_VALUES);
	if (!Null(args)) {
		asm_op(env, OP_PUSHVALUES);
		compile_body(env, args, FLAG_IGNORE);
		asm_op(env, OP_POPVALUES);
	}
	return FLAG_VALUES;
}


static int
c_multiple_value_setq(cl_env_ptr env, cl_object orig_args, int flags) {
	cl_object args = orig_args;
	cl_object orig_vars;
	cl_object vars = ECL_NIL, values;
	cl_object old_variables = env->c_env->variables;
	cl_index nvars = 0;

	/* Look for symbol macros, building the list of variables
	   and the list of late assignments. */
	for (orig_vars = pop(&args); !Null(orig_vars); ) {
		cl_object v = pop(&orig_vars);
		if (!ECL_SYMBOLP(v))
			FEillegal_variable_name(v);
		v = c_macro_expand1(env, v);
		if (!ECL_SYMBOLP(v)) {
			/* If any of the places to be set is not a variable,
			 * transform MULTIPLE-VALUE-SETQ into (SETF (VALUES ...))
			 */
			args = orig_args;
			return compile_form(env, cl_listX(3, @'setf',
                                                          CONS(@'values', CAR(args)),
                                                          CDR(args)),
					    flags);
		}
		vars = CONS(v, vars);
		nvars++;
	}

	/* Compile values */
	values = pop(&args);
	if (args != ECL_NIL)
		FEprogram_error_noreturn("MULTIPLE-VALUE-SETQ: Too many arguments.", 0);
	if (nvars == 0) {
		/* No variables */
		return compile_form(env, cl_list(2, @'values', values), flags);
	}
	compile_form(env, values, FLAG_VALUES);

	/* Compile variables */
	for (nvars = 0, vars = cl_nreverse(vars); vars != ECL_NIL; nvars++, vars = ECL_CONS_CDR(vars)) {
		if (nvars) {
			compile_setq(env, OP_VSETQ, ECL_CONS_CAR(vars));
			asm_arg(env, nvars);
		} else {
			compile_setq(env, OP_SETQ, ECL_CONS_CAR(vars));
		}
	}

	c_undo_bindings(env, old_variables, 0);

	return FLAG_REG0;
}

/*
	The OP_NOT operator reverses the boolean value of VALUES(0).
*/
static int
c_not(cl_env_ptr env, cl_object args, int flags) {
	flags = maybe_reg0(flags);
	if (flags & FLAG_USEFUL) {
		/* The value is useful */
		compile_form(env, pop(&args), FLAG_REG0);
		asm_op(env, OP_NOT);
	} else {
		/* The value may be ignored. */
		flags = compile_form(env, pop(&args), flags);
	}
	if (!Null(args))
		FEprogram_error_noreturn("NOT/NULL: Too many arguments.", 0);
	return flags;
}

/*
	The OP_NTHVAL operator moves a value from VALUES(ndx) to
	VALUES(0). The index NDX is taken from the stack.

		OP_NTHVAL
*/
static int
c_nth_value(cl_env_ptr env, cl_object args, int flags) {
	compile_form(env, pop(&args), FLAG_PUSH);	/* INDEX */
	compile_form(env, pop(&args), FLAG_VALUES);	/* VALUES */
	if (args != ECL_NIL)
		FEprogram_error_noreturn("NTH-VALUE: Too many arguments.",0);
	asm_op(env, OP_NTHVAL);
	return FLAG_REG0;
}


static int
c_prog1(cl_env_ptr env, cl_object args, int flags) {
	cl_object form = pop(&args);
	if (!(flags & FLAG_USEFUL) || (flags & FLAG_PUSH)) {
		flags = compile_form(env, form, flags);
		compile_body(env, args, FLAG_IGNORE);
	} else {
		flags = FLAG_REG0;
		compile_form(env, form, FLAG_PUSH);
		compile_body(env, args, FLAG_IGNORE);
		asm_op(env, OP_POP);
	}
	return flags;
}


/*
	The OP_PROGV operator exectures a set of statements in a lexical
	environment that has been extended with special variables. The
	list of special variables is taken from the top of the stack,
	while the list of values is in VALUES(0).

		...		; list of variables
		OP_PUSH
		...		; list of values
		OP_PROGV
		...		; body of progv
		OP_EXIT
*/
static int
c_progv(cl_env_ptr env, cl_object args, int flags) {
	cl_object vars = pop(&args);
	cl_object values = pop(&args);

	/* The list of variables is in the stack */
	compile_form(env, vars, FLAG_PUSH);

	/* The list of values is in reg0 */
	compile_form(env, values, FLAG_REG0);

	/* The body is interpreted within an extended lexical
	   environment. However, as all the new variables are
	   special, the compiler need not take care of them
	*/
	asm_op(env, OP_PROGV);
	flags = compile_body(env, args, FLAG_VALUES);
	asm_op(env, OP_EXIT_PROGV);

	return flags;
}


/*
	There are four assignment operators. They are

	1) Assign VALUES(0) to the lexical variable which occupies the
	   N-th position
		[OP_SETQ + n]

	2) Assign VALUES(0) to the special variable NAME
		[OP_SETQS + name]

	3) Pop a value from the stack and assign it to the lexical
	   variable in the N-th position.
		[OP_PSETQ + n]

	4) Pop a value from the stack and assign it to the special
	   variable denoted by NAME
		[OP_PSETQS + name]
*/
static int
c_psetq(cl_env_ptr env, cl_object old_args, int flags) {
	cl_object args = ECL_NIL, vars = ECL_NIL;
	bool use_psetf = FALSE;
	cl_index nvars = 0;

	if (Null(old_args))
		return compile_body(env, ECL_NIL, flags);
	/* We have to make sure that non of the variables which
	   are to be assigned is actually a symbol macro. If that
	   is the case, we invoke (PSETF ...) to handle the
	   macro expansions.
	*/
	do {
		cl_object var = pop(&old_args);
		cl_object value = pop(&old_args);
		if (!ECL_SYMBOLP(var))
			FEillegal_variable_name(var);
		var = c_macro_expand1(env, var);
		if (!ECL_SYMBOLP(var))
			use_psetf = TRUE;
		args = ecl_nconc(args, cl_list(2, var, value));
		nvars++;
	} while (!Null(old_args));
	if (use_psetf) {
		return compile_form(env, CONS(@'psetf', args), flags);
	}
	do {
		cl_object var = pop(&args);
		cl_object value = pop(&args);
		vars = CONS(var, vars);
		compile_form(env, value, FLAG_PUSH);
	} while (!Null(args));
	do {
		compile_setq(env, OP_PSETQ, pop(&vars));
        } while (!Null(vars));
	return compile_form(env, ECL_NIL, flags);
}


/*
	The OP_RETFROM operator returns from a block using the objects
	in VALUES() as output values.

		...		; output form
		OP_RETFROM
		tag		; object which names the block
*/
static int
c_return_aux(cl_env_ptr env, cl_object name, cl_object stmt, int flags)
{
	cl_object ndx = c_tag_ref(env, name, @':block');
	cl_object output = pop_maybe_nil(&stmt);

	if (!ECL_SYMBOLP(name) || Null(ndx))
		FEprogram_error_noreturn("RETURN-FROM: Unknown block name ~S.", 1, name);
	if (stmt != ECL_NIL)
		FEprogram_error_noreturn("RETURN-FROM: Too many arguments.", 0);
	compile_form(env, output, FLAG_VALUES);
	asm_op2(env, OP_RETURN, ecl_fixnum(ndx));
	return FLAG_VALUES;
}

static int
c_return(cl_env_ptr env, cl_object stmt, int flags) {
	return c_return_aux(env, ECL_NIL, stmt, flags);
}


static int
c_return_from(cl_env_ptr env, cl_object stmt, int flags) {
	cl_object name = pop(&stmt);
	return c_return_aux(env, name, stmt, flags);
}


static int
c_setq(cl_env_ptr env, cl_object args, int flags) {
	if (Null(args))
		return compile_form(env, ECL_NIL, flags);
	do {
		cl_object var = pop(&args);
		cl_object value = pop(&args);
		if (!ECL_SYMBOLP(var))
			FEillegal_variable_name(var);
		var = c_macro_expand1(env, var);
		if (ECL_SYMBOLP(var)) {
			flags = FLAG_REG0;
			compile_form(env, value, FLAG_REG0);
			compile_setq(env, OP_SETQ, var);
		} else {
			flags = ecl_endp(args)? FLAG_VALUES : FLAG_REG0;
			compile_form(env, cl_list(3, @'setf', var, value), flags);
		}
	} while (!Null(args));
	return flags;
}


static int
c_symbol_macrolet(cl_env_ptr env, cl_object args, int flags)
{
	cl_object def_list, specials, body;
	cl_object old_variables = env->c_env->variables;

	def_list = pop(&args);
	body = c_process_declarations(args);
	specials = env->values[3];

	/* Scan the list of definitions */
	while (!Null(def_list)) {
		cl_object definition = pop(&def_list);
		cl_object name = pop(&definition);
		cl_object expansion = pop(&definition);
		cl_object arglist = cl_list(2, @gensym(0), @gensym(0));
		cl_object function;
		if ((ecl_symbol_type(name) & (ecl_stp_constant|ecl_stp_special)) ||
                    ecl_member_eq(name, specials))
		{
			FEprogram_error_noreturn("SYMBOL-MACROLET: Symbol ~A cannot be \
declared special and appear in a symbol-macrolet.", 1, name);
		}
		definition = cl_list(2, arglist, cl_list(2, @'quote', expansion));
		function = ecl_make_lambda(env, name, definition);
		c_register_symbol_macro(env, name, function);
	}
	c_declare_specials(env, specials);
	flags = compile_toplevel_body(env, body, flags);
	c_undo_bindings(env, old_variables, 0);
	return flags;
}

static int
c_tagbody(cl_env_ptr env, cl_object args, int flags)
{
	cl_object old_env = env->c_env->variables;
	cl_index tag_base;
	cl_object labels = ECL_NIL, label, body;
	cl_type item_type;
	int nt, i;

	/* count the tags */
	for (nt = 0, body = args; !Null(body); ) {
		label = pop(&body);
		item_type = ecl_t_of(label);
		if (item_type == t_symbol || item_type == t_fixnum ||
	            item_type == t_bignum) {
			labels = CONS(CONS(label,ecl_make_fixnum(nt)), labels);
			nt += 1;
		}
	}
	if (nt == 0) {
		compile_body(env, args, 0);
		return compile_form(env, ECL_NIL, flags);
	}
	asm_op2c(env, OP_BLOCK, ecl_make_fixnum(0));
	c_register_tags(env, labels);
	asm_op2(env, OP_TAGBODY, nt);
	tag_base = current_pc(env);
	for (i = nt; i; i--)
		asm_arg(env, 0);

	for (body = args; !Null(body); ) {
		label = pop(&body);
		item_type = ecl_t_of(label);
		if (item_type == t_symbol || item_type == t_fixnum ||
	            item_type == t_bignum) {
			asm_complete(env, 0, tag_base);
			tag_base += OPARG_SIZE;
		} else {
			compile_form(env, label, FLAG_IGNORE);
		}
	}
	asm_op(env, OP_EXIT_TAGBODY);
	c_undo_bindings(env, old_env, 0);
	return FLAG_REG0;
}

static int
c_the(cl_env_ptr env, cl_object stmt, int flags) {
	cl_object type = pop(&stmt);
	cl_object value = pop(&stmt);
	if (stmt != ECL_NIL) {
		FEprogram_error_noreturn("THE: Too many arguments",0);
	}
	return compile_form(env, value, flags);
}

/*
	The OP_THROW jumps to an enclosing OP_CATCH whose tag
	matches the one of the throw. The tag is taken from the
	stack, while the output values are left in VALUES().
*/
static int
c_throw(cl_env_ptr env, cl_object stmt, int flags) {
	cl_object tag = pop(&stmt);
	cl_object form = pop(&stmt);
	if (stmt != ECL_NIL)
		FEprogram_error_noreturn("THROW: Too many arguments.",0);
	compile_form(env, tag, FLAG_PUSH);
	compile_form(env, form, FLAG_VALUES);
	asm_op(env, OP_THROW);
	return flags;
}


static int
c_unwind_protect(cl_env_ptr env, cl_object args, int flags) {
	cl_index label = asm_jmp(env, OP_PROTECT);

	flags = maybe_values(flags);

	/* Compile form to be protected */
	flags = compile_form(env, pop(&args), flags);
	asm_op(env, OP_PROTECT_NORMAL);

	/* Compile exit clause */
	asm_complete(env, OP_PROTECT, label);
	compile_body(env, args, FLAG_IGNORE);
	asm_op(env, OP_PROTECT_EXIT);

	return flags;
}


/*
	The OP_VALUES moves N values from the stack to VALUES().

		[OP_VALUES + n]
*/
static int
c_values(cl_env_ptr env, cl_object args, int flags) {
	if (!(flags & FLAG_USEFUL)) {
		/* This value will be discarded. We do not care to
		   push it or to save it in VALUES */
		if (Null(args))
			return flags;
		return compile_body(env, args, flags);
	} else if (flags & FLAG_PUSH) {
		/* We only need the first value. However, the rest
		   of arguments HAVE to be be evaluated */
		if (Null(args))
			return compile_form(env, ECL_NIL, flags);
		flags = compile_form(env, pop(&args), FLAG_PUSH);
		compile_body(env, args, FLAG_IGNORE);
		return flags;
	} else if (Null(args)) {
		asm_op(env, OP_NOP);
	} else {
		int n = 0;
		while (!Null(args)) {
			compile_form(env, pop_maybe_nil(&args), FLAG_PUSH);
			n++;
		}
		asm_op2(env, OP_VALUES, n);
	}
	return FLAG_VALUES;
}

static int
need_to_make_load_form_p(cl_object o)
{
        switch (ecl_t_of(o)) {
        case t_character:
        case t_fixnum:
        case t_bignum:
        case t_ratio:
        case t_singlefloat:
        case t_doublefloat:
#ifdef ECL_LONG_FLOAT
        case t_longfloat:
#endif
        case t_complex:
        case t_symbol:
        case t_pathname:
#ifdef ECL_UNICODE
        case t_string:
#endif
        case t_base_string:
        case t_bitvector:
                return 0;
        case t_list:
                if (Null(o)) return 0;
        default:
                return _ecl_funcall3(@'clos::need-to-make-load-form-p', o, ECL_NIL)
                        != ECL_NIL;
        }
}

static void
maybe_make_load_forms(cl_env_ptr env, cl_object constant)
{
        const cl_compiler_ptr c_env = env->c_env;
        cl_object init, make;
        if (c_env->mode != FLAG_LOAD && c_env->mode != FLAG_ONLY_LOAD)
                return;
        if (c_search_constant(env, constant) >= 0)
                return;
        if (!need_to_make_load_form_p(constant))
                return;
        make = _ecl_funcall2(@'make-load-form', constant);
        init = (env->nvalues > 1)? env->values[1] : ECL_NIL;
        c_env->load_time_forms = ecl_cons(cl_list(3, constant, make, init),
                                          c_env->load_time_forms);
}

static int
compile_constant(cl_env_ptr env, cl_object stmt, int flags)
{
        if (flags & FLAG_USEFUL) {
                bool push = flags & FLAG_PUSH;
                cl_fixnum n;
                maybe_make_load_forms(env, stmt);
                if (stmt == ECL_NIL) {
                        asm_op(env, push? OP_PUSHNIL : OP_NIL);
                } else if (ECL_FIXNUMP(stmt) && (n = ecl_fixnum(stmt)) <= MAX_OPARG
                           && n >= -MAX_OPARG) {
                        asm_op2(env, push? OP_PINT : OP_INT, n);
                } else {
                        asm_op2c(env, push? OP_PUSHQ : OP_QUOTE, stmt);
                }
                if (flags & FLAG_VALUES)
                        flags = (flags & ~FLAG_VALUES) | FLAG_REG0;
        }
        return flags;
}

static int
c_quote(cl_env_ptr env, cl_object args, int flags)
{
        if (ECL_ATOM(args) || ECL_CONS_CDR(args) != ECL_NIL)
                FEill_formed_input();
        return compile_constant(env, ECL_CONS_CAR(args), flags);
}

static int
compile_symbol(cl_env_ptr env, cl_object stmt, int flags)
{
        cl_object stmt1 = c_macro_expand1(env, stmt);
        if (stmt1 != stmt) {
                return compile_form(env, stmt1, flags);
        } else {
                cl_fixnum index = c_var_ref(env, stmt,0,FALSE);
                bool push = flags & FLAG_PUSH;
                if (index >= 0) {
                        asm_op2(env, push? OP_PUSHV : OP_VAR, index);
                } else {
                        asm_op2c(env, push? OP_PUSHVS : OP_VARS, stmt);
                }
                if (flags & FLAG_VALUES)
                        return (flags & ~FLAG_VALUES) | FLAG_REG0;
                else
                        return flags;
        }
}

static int
compile_form(cl_env_ptr env, cl_object stmt, int flags) {
        const cl_compiler_ptr c_env = env->c_env;
	cl_object function;
	int new_flags;

	ecl_bds_bind(env, @'si::*current-form*', stmt);
 BEGIN:
	if (c_env->code_walker != OBJNULL) {
		stmt = funcall(3, c_env->code_walker, stmt,
			       CONS(c_env->variables, c_env->macros));
	}
	/*
	 * First try with variable references and quoted constants
	 */
        if (Null(stmt)) {
                new_flags = compile_constant(env, stmt, flags);
                goto OUTPUT;
        }
        if (!ECL_LISTP(stmt)) {
                if (ECL_SYMBOLP(stmt)) {
                        new_flags = compile_symbol(env, stmt, flags);
                } else {
                        new_flags = compile_constant(env, stmt, flags);
                }
                goto OUTPUT;
	}
	/*
	 * Next try with special forms.
	 */
	function = ECL_CONS_CAR(stmt);
	if (ECL_SYMBOLP(function)) {
		cl_object index = ecl_gethash(function, cl_core.compiler_dispatch);
		if (index != OBJNULL) {
			compiler_record *l = database + ecl_fixnum(index);
			c_env->lexical_level += l->lexical_increment;
			if (c_env->stepping && function != @'function' &&
			    c_env->lexical_level)
				asm_op2c(env, OP_STEPIN, stmt);
			new_flags = (*(l->compiler))(env, ECL_CONS_CDR(stmt), flags);
			if (c_env->stepping && function != @'function' &&
			    c_env->lexical_level)
				asm_op(env, OP_STEPOUT);
			c_env->lexical_level -= l->lexical_increment;
			goto OUTPUT;
		}
                /*
                 * Next try to macroexpand
                 */
                {
                        cl_object new_stmt = c_macro_expand1(env, stmt);
                        if (new_stmt != stmt){
                                stmt = new_stmt;
                                goto BEGIN;
                        }
                }
        }
	/*
	 * Finally resort to ordinary function calls.
	 */
	if (c_env->stepping)
		asm_op2c(env, OP_STEPIN, stmt);
        c_env->lexical_level++;
	new_flags = c_call(env, stmt, flags);
        c_env->lexical_level--;
 OUTPUT:
	/*
		flags		new_flags		action
		PUSH		PUSH			---
		PUSH		VALUES			OP_PUSH
		PUSH		REG0			OP_PUSH
		VALUES		PUSH			Impossible
		VALUES		VALUES			---
		VALUES		REG0			OP_VALUEREG0
		REG0		PUSH			Impossible
		REG0		VALUES			---
		REG0		REG0			---
	*/
	if (flags & FLAG_PUSH) {
		if (new_flags & (FLAG_REG0 | FLAG_VALUES))
			asm_op(env, OP_PUSH);
	} else if (flags & FLAG_VALUES) {
		if (new_flags & FLAG_REG0) {
			asm_op(env, OP_VALUEREG0);
		} else if (new_flags & FLAG_PUSH) {
			FEerror("Internal error in bytecodes compiler", 0);
		}
	} else if (new_flags & FLAG_PUSH) {
		FEerror("Internal error in bytecodes compiler", 0);
	}
	ecl_bds_unwind1(env);
	return flags;
}

static void
eval_nontrivial_form(cl_env_ptr env, cl_object form) {
        const cl_compiler_ptr old_c_env = env->c_env;
        struct cl_compiler_env new_c_env = *old_c_env;
        cl_index handle;
        cl_object bytecodes;
        struct ecl_stack_frame frame;
        frame.t = t_frame;
        frame.stack = frame.base = 0;
        frame.size = 0;
        frame.env = env;
        env->nvalues = 0;
        env->values[0] = ECL_NIL;
        new_c_env.constants = si_make_vector(ECL_T, ecl_make_fixnum(16),
					     ECL_T, /* Adjustable */
					     ecl_make_fixnum(0), /* Fillp */
					     ECL_NIL, /* displacement */
					     ECL_NIL);
        new_c_env.load_time_forms = ECL_NIL;
        new_c_env.env_depth = 0;
        new_c_env.env_size = 0;
        env->c_env = &new_c_env;
        handle = asm_begin(env);
        compile_with_load_time_forms(env, form, FLAG_VALUES);
        if (current_pc(env) != handle) {
                asm_op(env, OP_EXIT);
                bytecodes = asm_end(env, handle, form);
                env->values[0] = ecl_interpret((cl_object)&frame,
                                               new_c_env.lex_env,
                                               bytecodes);
#ifdef GBC_BOEHM
                GC_free(bytecodes->bytecodes.code);
                GC_free(bytecodes);
#endif
        }
        env->c_env = old_c_env;
}

static void
eval_form(cl_env_ptr env, cl_object form) {
        if (ECL_LISTP(form) || ECL_SYMBOLP(form)) {
                eval_nontrivial_form(env, form);
        } else {
                env->values[0] = form;
                env->nvalues = 1;
        }
}

static int
execute_each_form(cl_env_ptr env, cl_object body)
{
        cl_object form = ECL_NIL, next_form;
        for (form = ECL_NIL; !Null(body); form = next_form) {
                unlikely_if (!ECL_LISTP(body))
                        FEtype_error_proper_list(body);
                next_form = ECL_CONS_CAR(body);
                body = ECL_CONS_CDR(body);
                eval_form(env, form);
        }
        eval_form(env, form);
        return FLAG_VALUES;
}

static cl_index *
save_bytecodes(cl_env_ptr env, cl_index start, cl_index end)
{
#ifdef GBC_BOEHM
        cl_index l = end - start;
        cl_index *bytecodes = ecl_alloc_atomic((l + 1) * sizeof(cl_index));
        cl_index *p = bytecodes;
        for (*(p++) = l; end > start; end--, p++) {
                *p = (cl_index)ECL_STACK_POP_UNSAFE(env);
        }
        return bytecodes;
#else
#error "Pointer references outside of recognizable object"
#endif
}

static void
restore_bytecodes(cl_env_ptr env, cl_index *bytecodes)
{
        cl_index *p = bytecodes;
        cl_index l;
        for (l = *p; l; l--) {
                ECL_STACK_PUSH(env, (cl_object)p[l]);
        }
        ecl_dealloc(bytecodes);
}

static int
compile_with_load_time_forms(cl_env_ptr env, cl_object form, int flags)
{
        /*
         * First compile the form as usual.
         */
        const cl_compiler_ptr c_env = env->c_env;
        cl_index handle = asm_begin(env);
        int output_flags = compile_form(env, form, flags);
        /*
         * If some constants need to be built, we insert the
         * code _before_ the actual forms;
         */
        if (c_env->load_time_forms != ECL_NIL) {
                cl_index *bytecodes = save_bytecodes(env, handle, current_pc(env));
                cl_object p, forms_list = c_env->load_time_forms;
		c_env->load_time_forms = ECL_NIL;
		p = forms_list;
                do {
                        cl_object r = ECL_CONS_CAR(p);
                        cl_object constant = pop(&r);
                        cl_object make_form = pop(&r);
                        cl_object init_form = pop(&r);
                        cl_index loc = c_register_constant(env, constant);
                        compile_with_load_time_forms(env, make_form, FLAG_REG0);
                        asm_op2(env, OP_CSET, loc);
                        compile_with_load_time_forms(env, init_form, FLAG_IGNORE);
			ECL_RPLACA(p, ecl_make_fixnum(loc));
                        p = ECL_CONS_CDR(p);
                } while (p != ECL_NIL);
		p = forms_list;
		do {
			cl_index loc = ecl_fixnum(ECL_CONS_CAR(p));
			/* Clear created constants (they cannot be printed) */
			c_env->constants->vector.self.t[loc] = ecl_make_fixnum(0);
			p = ECL_CONS_CDR(p);
		} while (p != ECL_NIL);
                restore_bytecodes(env, bytecodes);
        }
        return output_flags;
}

static int
compile_each_form(cl_env_ptr env, cl_object body, int flags)
{
        cl_object form = ECL_NIL, next_form;
        for (form = ECL_NIL; !Null(body); form = next_form) {
                unlikely_if (!ECL_LISTP(body))
                        FEtype_error_proper_list(body);
                next_form = ECL_CONS_CAR(body);
                body = ECL_CONS_CDR(body);
                compile_with_load_time_forms(env, form, FLAG_IGNORE);
        }
        return compile_with_load_time_forms(env, form, flags);
}

static int
compile_toplevel_body(cl_env_ptr env, cl_object body, int flags)
{
        const cl_compiler_ptr c_env = env->c_env;
        if (!c_env->lexical_level) {
                if (c_env->mode == FLAG_EXECUTE)
                        return execute_each_form(env, body);
                else
                        return compile_each_form(env, body, flags);
        } else {
                return compile_body(env, body, flags);
        }
}

static int
compile_body(cl_env_ptr env, cl_object body, int flags)
{
        cl_object form = ECL_NIL, next_form;
        for (form = ECL_NIL; !Null(body); form = next_form) {
                unlikely_if (!ECL_LISTP(body))
                        FEtype_error_proper_list(body);
                next_form = ECL_CONS_CAR(body);
                body = ECL_CONS_CDR(body);
                compile_form(env, form, FLAG_IGNORE);
        }
        return compile_form(env, form, flags);
}

/* ------------------------ INLINED FUNCTIONS -------------------------------- */

static int
c_cons(cl_env_ptr env, cl_object args, int flags)
{
	if (ecl_length(args) != 2) {
		FEprogram_error_noreturn("CONS: Wrong number of arguments", 0);
	}
	compile_form(env, cl_first(args), FLAG_PUSH);
	compile_form(env, cl_second(args), FLAG_REG0);
	asm_op(env, OP_CONS);
	return FLAG_REG0;
}

static int
c_endp(cl_env_ptr env, cl_object args, int flags)
{
	cl_object list = pop(&args);
	if (args != ECL_NIL) {
		FEprogram_error_noreturn("ENDP: Too many arguments", 0);
	}
	compile_form(env, list, FLAG_REG0);
	asm_op(env, OP_ENDP);
	return FLAG_REG0;
}

static int
c_car(cl_env_ptr env, cl_object args, int flags)
{
	cl_object list = pop(&args);
	if (args != ECL_NIL) {
		FEprogram_error_noreturn("CAR: Too many arguments", 0);
	}
	compile_form(env, list, FLAG_REG0);
	asm_op(env, OP_CAR);
	return FLAG_REG0;
}

static int
c_cdr(cl_env_ptr env, cl_object args, int flags)
{
	cl_object list = pop(&args);
	if (args != ECL_NIL) {
		FEprogram_error_noreturn("CDR: Too many arguments", 0);
	}
	compile_form(env, list, FLAG_REG0);
	asm_op(env, OP_CDR);
	return FLAG_REG0;
}

static int
c_list_listA(cl_env_ptr env, cl_object args, int flags, int op)
{
	cl_index n = ecl_length(args);
	if (n == 0) {
		return compile_form(env, ECL_NIL, flags);
	} else {
		while (ECL_CONS_CDR(args) != ECL_NIL) {
			compile_form(env, ECL_CONS_CAR(args), FLAG_PUSH);
			args = ECL_CONS_CDR(args);
		}
		compile_form(env, ECL_CONS_CAR(args), FLAG_REG0);
		asm_op2(env, op, n);
		return FLAG_REG0;
	}
}

static int
c_list(cl_env_ptr env, cl_object args, int flags)
{
	return c_list_listA(env, args, flags, OP_LIST);
}

static int
c_listA(cl_env_ptr env, cl_object args, int flags)
{
	return c_list_listA(env, args, flags, OP_LISTA);
}


/* ----------------------------- PUBLIC INTERFACE ---------------------------- */

/* ------------------------------------------------------------
   LAMBDA OBJECTS: An interpreted function is a vector made of
	the following components

      #(LAMBDA
	{block-name | NIL}
	{variable-env | NIL}
	{function-env | NIL}
	{block-env | NIL}
	(list of variables declared special)
	Nreq {var}*			; required arguments
	Nopt {var value flag}*		; optional arguments
	{rest-var NIL}			; rest variable
	{T | NIL}			; allow other keys?
	Nkey {key var value flag}*	; keyword arguments
	Naux {var init}			; auxiliary variables
	documentation-string
	list-of-declarations
	{form}*				; body)

   ------------------------------------------------------------ */

/*
  Handles special declarations, removes declarations from body
 */
@(defun si::process-declarations (body &optional doc)
	cl_object documentation = ECL_NIL, declarations = ECL_NIL, specials = ECL_NIL;
@
	for (; !Null(body); body = ECL_CONS_CDR(body)) {
                cl_object form;
                unlikely_if (!ECL_LISTP(body))
                        FEill_formed_input();
                form = ECL_CONS_CAR(body);
                if (!Null(doc) && ecl_stringp(form) && !Null(ECL_CONS_CDR(body))) {
                        if (documentation != ECL_NIL)
                                break;
                        documentation = form;
                        continue;
                }
                if (ECL_ATOM(form) || (ECL_CONS_CAR(form) != @'declare')) {
                        break;
                }
                for (form = ECL_CONS_CDR(form); !Null(form); ) {
                        cl_object sentence = pop(&form);
                        declarations = ecl_cons(sentence, declarations);
                        if (pop(&sentence) == @'special') {
                                while (!Null(sentence)) {
                                        cl_object v = pop(&sentence);
                                        assert_type_symbol(v);
                                        specials = ecl_cons(v, specials);
                                }
                        }
                }
	}
	@(return cl_nreverse(declarations) body documentation specials)
@)

cl_object
si_process_lambda(cl_object lambda)
{
	cl_object documentation, declarations, specials;
	cl_object lambda_list, body;
        const cl_env_ptr env = ecl_process_env();
	unlikely_if (ECL_ATOM(lambda))
		FEprogram_error_noreturn("LAMBDA: No lambda list.", 0);

	lambda_list = ECL_CONS_CAR(lambda);
        body = ECL_CONS_CDR(lambda);
	declarations = @si::process-declarations(2, body, ECL_T);
	body = env->values[1];
	documentation = env->values[2];
	specials = env->values[3];

        lambda_list = si_process_lambda_list(lambda_list, @'function');
        {
        cl_index n = env->nvalues;
	env->values[0] = lambda_list;
	env->values[n++] = documentation;
	env->values[n++] = specials;
	env->values[n++] = declarations;
	env->values[n++] = body;
        env->nvalues = n;
        }
	return lambda_list;
}

/*
 * (si::process-lambda-list lambda-list context)
 *
 * Parses different types of lambda lists. CONTEXT may be MACRO,
 * FTYPE, FUNCTION, METHOD or DESTRUCTURING-BIND, and determines the
 * valid sytax. The output is made of several values:
 *
 * VALUES(0) = (N req1 ... )			; required values
 * VALUES(1) = (N opt1 init1 flag1 ... )	; optional values
 * VALUES(2) = rest-var				; rest-variable, if any
 * VALUES(3) = key-flag				; T if &key was supplied
 * VALUES(4) = (N key1 var1 init1 flag1 ... )	; keyword arguments
 * VALUES(5) = allow-other-keys			; flag &allow-other-keys
 * VALUES(6) = (N aux1 init1 ... )		; auxiliary variables
 *
 * 1°) The prefix "N" is an integer value denoting the number of
 * variables which are declared within this section of the lambda
 * list.
 *
 * 2°) The INIT* arguments are lisp forms which are evaluated when
 * no value is provided.
 *
 * 3°) The FLAG* arguments is the name of a variable which holds a
 * boolean value in case an optional or keyword argument was
 * provided. If it is NIL, no such variable exists.
 */

cl_object
si_process_lambda_list(cl_object org_lambda_list, cl_object context)
{
#define push(v,l) { cl_object c = *l = CONS(v, *l); l = &ECL_CONS_CDR(c); }
#define assert_var_name(v) \
	if (context == @'function') { \
		unlikely_if (ecl_symbol_type(v) & ecl_stp_constant)	\
			FEillegal_variable_name(v); }
        cl_object lists[4] = {ECL_NIL, ECL_NIL, ECL_NIL, ECL_NIL};
        cl_object *reqs = lists, *opts = lists+1, *keys = lists+2, *auxs = lists+3;
	cl_object v, rest = ECL_NIL, lambda_list = org_lambda_list;
	int nreq = 0, nopt = 0, nkey = 0, naux = 0;
	cl_object allow_other_keys = ECL_NIL;
	cl_object key_flag = ECL_NIL;
        enum {  AT_REQUIREDS,
                AT_OPTIONALS,
                AT_REST,
                AT_KEYS,
                AT_OTHER_KEYS,
                AT_AUXS
        } stage = AT_REQUIREDS;

	if (!ECL_LISTP(lambda_list))
		goto ILLEGAL_LAMBDA;
LOOP:
        if (Null(lambda_list))
                goto OUTPUT;
	if (!ECL_LISTP(lambda_list)) {
		unlikely_if (context == @'function' || context == @'ftype')
			goto ILLEGAL_LAMBDA;
                v = lambda_list;
                lambda_list = ECL_NIL;
                goto REST;
	}
	v = ECL_CONS_CAR(lambda_list);
	lambda_list = ECL_CONS_CDR(lambda_list);
	if (v == @'&optional') {
		unlikely_if (stage >= AT_OPTIONALS)
			goto ILLEGAL_LAMBDA;
		stage = AT_OPTIONALS;
		goto LOOP;
	}
	if (v == @'&rest' || (v == @'&body' && (context == @'si::macro' || context == @'destructuring-bind'))) {
		unlikely_if (ECL_ATOM(lambda_list))
                        goto ILLEGAL_LAMBDA;
		v = ECL_CONS_CAR(lambda_list);
		lambda_list = ECL_CONS_CDR(lambda_list);
REST:		unlikely_if (stage >= AT_REST)
			goto ILLEGAL_LAMBDA;
		stage = AT_REST;
		rest = v;
		goto LOOP;
	}
	if (v == @'&key') {
		unlikely_if (stage >= AT_KEYS)
			goto ILLEGAL_LAMBDA;
		key_flag = ECL_T;
		stage = AT_KEYS;
		goto LOOP;
	}
	if (v == @'&aux') {
		unlikely_if (stage >= AT_AUXS)
			goto ILLEGAL_LAMBDA;
		stage = AT_AUXS;
		goto LOOP;
	}
	if (v == @'&allow-other-keys') {
		allow_other_keys = ECL_T;
		unlikely_if (stage != AT_KEYS)
			goto ILLEGAL_LAMBDA;
		stage = AT_OTHER_KEYS;
		goto LOOP;
	}
	switch (stage) {
	case AT_REQUIREDS:
		nreq++;
                assert_var_name(v);
		push(v, reqs);
		break;
	case AT_OPTIONALS: {
		cl_object spp = ECL_NIL;
		cl_object init = ECL_NIL;
		if (!ECL_ATOM(v) && (context != @'ftype')) {
			cl_object x = v;
                        unlikely_if (!ECL_LISTP(x)) goto ILLEGAL_LAMBDA;
			v = ECL_CONS_CAR(x);
                        x = ECL_CONS_CDR(x);
			if (!Null(x)) {
                                unlikely_if (!ECL_LISTP(x)) goto ILLEGAL_LAMBDA;
				init = ECL_CONS_CAR(x);
                                x = ECL_CONS_CDR(x);
				if (!Null(x)) {
                                        unlikely_if (!ECL_LISTP(x)) goto ILLEGAL_LAMBDA;
					spp = ECL_CONS_CAR(x);
                                        x = ECL_CONS_CDR(x);
                                        if (spp != ECL_NIL) assert_var_name(spp);
					unlikely_if (!Null(x))
						goto ILLEGAL_LAMBDA;
				}
			}
		}
		nopt++;
                assert_var_name(v);
		push(v, opts);
		push(init, opts);
                push(spp, opts);
		break;
        }
	case AT_REST:
		/* If we get here, the user has declared more than one
		 * &rest variable, as in (lambda (&rest x y) ...) */
		goto ILLEGAL_LAMBDA;
	case AT_KEYS: {
		cl_object init = ECL_NIL;
		cl_object spp = ECL_NIL;
                cl_object key;
                if (context == @'ftype') {
                        unlikely_if (ECL_ATOM(v))
                                goto ILLEGAL_LAMBDA;
                        key = ECL_CONS_CAR(v);
                        v = CADR(v);
                        goto KEY_PUSH;
                }
		if (!ECL_ATOM(v)) {
			cl_object x = v;
			v = ECL_CONS_CAR(x);
                        x = ECL_CONS_CDR(x);
			if (!Null(x)) {
                                unlikely_if (!ECL_LISTP(x)) goto ILLEGAL_LAMBDA;
				init = ECL_CONS_CAR(x);
                                x = ECL_CONS_CDR(x);
				if (!Null(x)) {
                                        unlikely_if (!ECL_LISTP(x)) goto ILLEGAL_LAMBDA;
					spp = ECL_CONS_CAR(x);
                                        x = ECL_CONS_CDR(x);
					unlikely_if (!Null(x))
						goto ILLEGAL_LAMBDA;
                                        if (spp != ECL_NIL) assert_var_name(spp);
				}
			}
		}
		if (CONSP(v)) {
			key = ECL_CONS_CAR(v);
                        v = ECL_CONS_CDR(v);
			unlikely_if (ECL_ATOM(v) || !Null(ECL_CONS_CDR(v)))
				goto ILLEGAL_LAMBDA;
			v = ECL_CONS_CAR(v);
			if (context == @'function')
				assert_type_symbol(v);
			assert_type_symbol(key);
		} else {
			int intern_flag;
			key = ecl_intern(ecl_symbol_name(v), cl_core.keyword_package,
					 &intern_flag);
		}
        KEY_PUSH:
		nkey++;
		push(key, keys);
                assert_var_name(v);
		push(v, keys);
		push(init, keys);
                push(spp, keys);
		break;
        }
	default: {
                cl_object init;
		if (ECL_ATOM(v)) {
			init = ECL_NIL;
		} else if (Null(CDDR(v))) {
			cl_object x = v;
			v = ECL_CONS_CAR(x);
			init = CADR(x);
		} else
			goto ILLEGAL_LAMBDA;
		naux++;
                assert_var_name(v);
		push(v, auxs);
		push(init, auxs);
        }
	}
	goto LOOP;

OUTPUT:
	if ((nreq+nopt+(!Null(rest))+nkey) >= ECL_CALL_ARGUMENTS_LIMIT)
		FEprogram_error_noreturn("LAMBDA: Argument list ist too long, ~S.", 1,
				org_lambda_list);
	@(return CONS(ecl_make_fixnum(nreq), lists[0])
		 CONS(ecl_make_fixnum(nopt), lists[1])
		 rest
		 key_flag
		 CONS(ecl_make_fixnum(nkey), lists[2])
		 allow_other_keys
		 lists[3])

ILLEGAL_LAMBDA:
	FEprogram_error_noreturn("LAMBDA: Illegal lambda list ~S.", 1, org_lambda_list);

#undef push
#undef assert_var_name
}

static void
c_default(cl_env_ptr env, cl_object var, cl_object stmt, cl_object flag, cl_object specials)
{
        /* Flag is in REG0, value, if it exists, in stack */
        cl_index label;
        label = asm_jmp(env, OP_JT);
        compile_form(env, stmt, FLAG_PUSH);
        if (Null(flag)) {
                asm_complete(env, OP_JT, label);
        } else {
                compile_form(env, ECL_NIL, FLAG_REG0);
                asm_complete(env, OP_JT, label);
                c_bind(env, flag, specials);
        }
        c_pbind(env, var, specials);
}

cl_object
ecl_make_lambda(cl_env_ptr env, cl_object name, cl_object lambda) {
	cl_object reqs, opts, rest, key, keys, auxs, allow_other_keys;
	cl_object specials, doc, decl, body, output;
	cl_index handle;
	struct cl_compiler_env *old_c_env, new_c_env;

	ecl_bds_bind(env, @'si::*current-form*',
		     @list*(3, @'ext::lambda-block', name, lambda));

	old_c_env = env->c_env;
	c_new_env(env, &new_c_env, ECL_NIL, old_c_env);
	new_c_env.lexical_level++;

	reqs = si_process_lambda(lambda);
	opts = env->values[1];
	rest = env->values[2];
	key  = env->values[3];
	keys = env->values[4];
	allow_other_keys = env->values[5];
	auxs = env->values[6];
	doc  = env->values[7];
	specials = env->values[8];
	decl = env->values[9];
	body = env->values[10];

	handle = asm_begin(env);

	/* Transform (SETF fname) => fname */
	if (!Null(name) && Null(si_valid_function_name_p(name)))
		FEprogram_error_noreturn("LAMBDA: Not a valid function name ~S",1,name);

	/* We register as special variable a symbol which is not
	 * to be used. We use this to mark the boundary of a function
	 * environment and when code-walking */
	c_register_var(env, @'si::function-boundary', TRUE, FALSE);

	reqs = ECL_CONS_CDR(reqs);		/* Required arguments */
	while (!Null(reqs)) {
		cl_object var = pop(&reqs);
                asm_op(env, OP_POPREQ);
                c_bind(env, var, specials);
	}
        opts = ECL_CONS_CDR(opts);
        while (!Null(opts)) {			/* Optional arguments */
                cl_object var = pop(&opts);
                cl_object stmt = pop(&opts);
                cl_object flag = pop(&opts);
                asm_op(env, OP_POPOPT);
                c_default(env, var, stmt, flag, specials);
        }
        if (Null(rest) && Null(key)) {		/* Check no excess arguments */
                asm_op(env, OP_NOMORE);
        }
        if (!Null(rest)) {			/* &rest argument */
                asm_op(env, OP_POPREST);
                c_bind(env, rest, specials);
        }
        if (!Null(key)) {
                cl_object aux = CONS(allow_other_keys,ECL_NIL);
                cl_object names = ECL_NIL;
                asm_op2c(env, OP_PUSHKEYS, aux);
                keys = ECL_CONS_CDR(keys);
                while (!Null(keys)) {
                        cl_object name = pop(&keys);
                        cl_object var = pop(&keys);
                        cl_object stmt = pop(&keys);
                        cl_object flag = pop(&keys);
                        names = CONS(name, names);
                        asm_op(env, OP_POP);
                        c_default(env, var, stmt, flag, specials);
                }
                ECL_RPLACD(aux, names);
        }

	while (!Null(auxs)) {			/* Local bindings */
		cl_object var = pop(&auxs);
		cl_object value = pop(&auxs);
		compile_form(env, value, FLAG_REG0);
		c_bind(env, var, specials);
	}
	c_declare_specials(env, specials);

	if (!Null(name)) {
		compile_form(env, @list*(3, @'block', si_function_block_name(name),
                                         body), FLAG_VALUES);
	} else {
		while (!Null(decl)) {
			cl_object l = ECL_CONS_CAR(decl);
			if (ECL_CONSP(l) && ECL_CONS_CAR(l) == @'si::function-block-name') {
				name = ECL_CONS_CAR(ECL_CONS_CDR(l));
				break;
			}
			decl = ECL_CONS_CDR(decl);
		}
		compile_body(env, body, FLAG_VALUES);
	}

        /* Only undo special bindings */
	c_undo_bindings(env, old_c_env->variables, 1);
	asm_op(env, OP_EXIT);

        if (Null(ecl_symbol_value(@'si::*keep-definitions*')))
                lambda = ECL_NIL;
	output = asm_end(env, handle, lambda);
	output->bytecodes.name = name;

	old_c_env->load_time_forms = env->c_env->load_time_forms;
	env->c_env = old_c_env;

	ecl_bds_unwind1(env);

	return output;
}

static cl_object
ecl_function_block_name(cl_object name)
{
	if (ECL_SYMBOLP(name)) {
		return name;
	} else if (CONSP(name) && ECL_CONS_CAR(name) == @'setf') {
		name = ECL_CONS_CDR(name);
		if (CONSP(name)) {
                        cl_object output = ECL_CONS_CAR(name);
                        if (ECL_SYMBOLP(output) && Null(ECL_CONS_CDR(name)))
                                return output;
                }
        }
        return NULL;
}

cl_object
si_function_block_name(cl_object name)
{
        cl_object output = ecl_function_block_name(name);
        if (!output)
                FEinvalid_function_name(name);
        @(return output)
}

cl_object
si_valid_function_name_p(cl_object name)
{
        name = ecl_function_block_name(name);
        @(return (name? ECL_T : ECL_NIL))
}

cl_object
si_make_lambda(cl_object name, cl_object rest)
{
	cl_object lambda;
        const cl_env_ptr the_env = ecl_process_env();
	volatile cl_compiler_env_ptr old_c_env = the_env->c_env;
	struct cl_compiler_env new_c_env;

	c_new_env(the_env, &new_c_env, ECL_NIL, 0);
	ECL_UNWIND_PROTECT_BEGIN(the_env) {
		lambda = ecl_make_lambda(the_env, name, rest);
	} ECL_UNWIND_PROTECT_EXIT {
		the_env->c_env = old_c_env;
	} ECL_UNWIND_PROTECT_END;
	@(return lambda)
}

@(defun si::eval-with-env (form &optional (env ECL_NIL) (stepping ECL_NIL)
                           (compiler_env_p ECL_NIL) (execute ECL_T))
	volatile cl_compiler_env_ptr old_c_env;
	struct cl_compiler_env new_c_env;
	cl_object interpreter_env, compiler_env;
@
	/*
	 * Compile to bytecodes.
	 */
	if (compiler_env_p == ECL_NIL) {
		interpreter_env = env;
		compiler_env = ECL_NIL;
	} else {
		interpreter_env = ECL_NIL;
		compiler_env = env;
	}
	old_c_env = the_env->c_env;
	c_new_env(the_env, &new_c_env, compiler_env, 0);
	guess_environment(the_env, interpreter_env);
	new_c_env.lex_env = env;
	new_c_env.stepping = stepping != ECL_NIL;
	ECL_UNWIND_PROTECT_BEGIN(the_env) {
                if (Null(execute)) {
                        cl_index handle = asm_begin(the_env);
                        new_c_env.mode = FLAG_LOAD;
                        /*cl_print(1,form);*/
                        compile_with_load_time_forms(the_env, form, FLAG_VALUES);
                        asm_op(the_env, OP_EXIT);
                        the_env->values[0] = asm_end(the_env, handle, form);
                        the_env->nvalues = 1;
                } else {
                        eval_form(the_env, form);
                }
	} ECL_UNWIND_PROTECT_EXIT {
		/* Clear up */
		the_env->c_env = old_c_env;
		memset(&new_c_env, 0, sizeof(new_c_env));
	} ECL_UNWIND_PROTECT_END;
	return the_env->values[0];
@)

void
init_compiler()
{
	cl_object dispatch_table =
		cl_core.compiler_dispatch =
		cl__make_hash_table(@'eq', ecl_make_fixnum(128), /* size */
                                    cl_core.rehash_size,
                                    cl_core.rehash_threshold);
	int i;
	for (i = 0; database[i].symbol; i++) {
		ecl_sethash(database[i].symbol, dispatch_table, ecl_make_fixnum(i));
	}
}
