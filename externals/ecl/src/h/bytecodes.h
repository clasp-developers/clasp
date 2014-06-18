/* -*- mode: c; c-basic-offset: 8 -*- */
/**********************************************************************
 ***
 ***  IMPORTANT: ANY CHANGE IN THIS FILE MUST BE MATCHED BY
 ***		 APPROPRIATE CHANGES IN THE INTERPRETER AND COMPILER
 ***		 IN PARTICULAR, IT MAY HURT THE THREADED INTERPRETER
 ***		 CODE.
 **********************************************************************/
/*
 * See ecl/src/c/interpreter.d for a detailed explanation of all opcodes
 */
enum {
  OP_NOP,
  OP_QUOTE,
  OP_ENDP,
  OP_CONS,
  OP_CAR,
  OP_CDR,
  OP_LIST,
  OP_LISTA,
  OP_INT,
  OP_PINT,
  OP_VAR,
  OP_VARS,
  OP_PUSH,
  OP_PUSHV,
  OP_PUSHVS,
  OP_PUSHQ,
  OP_CALLG1,
  OP_CALLG2,
  OP_CALL,
  OP_CALLG,
  OP_FCALL,
  OP_MCALL,
  OP_POP,
  OP_POP1,
  OP_POPREQ,
  OP_POPOPT,
  OP_NOMORE,
  OP_POPREST,
  OP_PUSHKEYS,
  OP_EXIT,
  OP_FLET,
  OP_LABELS,
  OP_LFUNCTION,
  OP_FUNCTION,
  OP_CLOSE,
  OP_GO,
  OP_RETURN,
  OP_THROW,
  OP_JMP,
  OP_JNIL,
  OP_JT,
  OP_JEQL,
  OP_JNEQL,
  OP_UNBIND,
  OP_UNBINDS,
  OP_BIND,
  OP_PBIND,
  OP_VBIND,
  OP_BINDS,
  OP_PBINDS,
  OP_VBINDS,
  OP_SETQ,
  OP_SETQS,
  OP_PSETQ,
  OP_PSETQS,
  OP_VSETQ,
  OP_VSETQS,
  OP_BLOCK,
  OP_DO,
  OP_CATCH,
  OP_FRAME,
  OP_TAGBODY,
  OP_EXIT_TAGBODY,
  OP_EXIT_FRAME,
  OP_PROTECT,
  OP_PROTECT_NORMAL,
  OP_PROTECT_EXIT,
  OP_PROGV,
  OP_EXIT_PROGV,
  OP_PUSHVALUES,
  OP_POPVALUES,
  OP_PUSHMOREVALUES,
  OP_VALUES,
  OP_VALUEREG0,
  OP_NTHVAL,
  OP_NIL,
  OP_NOT,
  OP_PUSHNIL,
  OP_CSET,
  OP_STEPIN,
  OP_STEPCALL,
  OP_STEPOUT,
  OP_MAXOPCODES = 128,
  OP_OPCODE_SHIFT = 7
};

#define MAX_OPARG 0x7FFF
typedef int16_t cl_oparg;

/*
 * Note that in the small bytecodes case, we have to recompose a signed
 * small integer out of its pieces. We have to be careful because the
 * least significant byte has to be interpreted as unsigned, while the
 * most significant byte carries a sign.
 */
#ifdef ECL_SMALL_BYTECODES
  typedef signed char cl_opcode;
# define OPCODE_SIZE 1
# define OPARG_SIZE 2
# ifdef WORDS_BIGENDIAN
#  define READ_OPARG(v)	((cl_fixnum)v[0] << 8) + (unsigned char)v[1]
# else
#  define READ_OPARG(v) ((cl_fixnum)v[1] << 8) + (unsigned char)v[0]
# endif
# define GET_OPARG(r,v) { r = READ_OPARG(v); v += 2; }
#else
  typedef int16_t cl_opcode;
# define OPCODE_SIZE 1
# define OPARG_SIZE 1
# define READ_OPCODE(v)	v[0]
# define READ_OPARG(v)	v[0]
# define GET_OPARG(r,v) { r = *(v++); }
#endif
#define GET_OPCODE(v) *((v)++)
#define GET_DATA(r,v,data) { \
	cl_oparg ndx; \
	GET_OPARG(ndx, v); \
	r = data[ndx]; \
}
#define GET_DATA_PTR(r,v,data) { \
	cl_oparg ndx; \
	GET_OPARG(ndx, v); \
	r = data+ndx; \
}
#define GET_LABEL(pc,v)	{ \
	pc = (v) + READ_OPARG(v); \
	v += OPARG_SIZE; \
}

/**********************************************************************
 * THREADED INTERPRETER CODE
 *
 * By using labels as values, we can build a variant of the
 * interpreter code that leads to better performance because (i) it
 * saves a range check on the opcode size and (ii) each opcode has a
 * dispatch instruction at the end, so that the processor may better
 * predict jumps.
 *
 * NOTE: We cannot use this in Solaris because GCC erroneously produces
 * relocation tables.
 */
#if (defined(__GNUC__) && !defined(__STRICT_ANSI__) && !defined(__clang__) && !defined(__llvm__))
#define ECL_THREADED_INTERPRETER
# if defined(__sun__) && (FIXNUM_BITS > 32)
#  undef ECL_THREADED_INTERPRETER
# endif
#endif

#ifdef ECL_THREADED_INTERPRETER
#define BEGIN_SWITCH \
	THREAD_NEXT;
#define CASE(name) \
	LBL_##name:
#define THREAD_NEXT \
	goto *(&&LBL_OP_NOP + offsets[GET_OPCODE(vector)])
#else
#define BEGIN_SWITCH \
	switch (GET_OPCODE(vector))
#define THREAD_NEXT \
	goto BEGIN
#define CASE(name) \
	case name:
#endif

#if !defined(ECL_THREADED_INTERPRETER)
#define ECL_OFFSET_TABLE
#else
#define ECL_OFFSET_TABLE \
	static const int offsets[] = {\
  &&LBL_OP_NOP - &&LBL_OP_NOP,\
  &&LBL_OP_QUOTE - &&LBL_OP_NOP,\
  &&LBL_OP_ENDP - &&LBL_OP_NOP,\
  &&LBL_OP_CONS - &&LBL_OP_NOP,\
  &&LBL_OP_CAR - &&LBL_OP_NOP,\
  &&LBL_OP_CDR - &&LBL_OP_NOP,\
  &&LBL_OP_LIST - &&LBL_OP_NOP,\
  &&LBL_OP_LISTA - &&LBL_OP_NOP,\
  &&LBL_OP_INT - &&LBL_OP_NOP,\
  &&LBL_OP_PINT - &&LBL_OP_NOP,\
  &&LBL_OP_VAR - &&LBL_OP_NOP,\
  &&LBL_OP_VARS - &&LBL_OP_NOP,\
  &&LBL_OP_PUSH - &&LBL_OP_NOP,\
  &&LBL_OP_PUSHV - &&LBL_OP_NOP,\
  &&LBL_OP_PUSHVS - &&LBL_OP_NOP,\
  &&LBL_OP_PUSHQ - &&LBL_OP_NOP,\
  &&LBL_OP_CALLG1 - &&LBL_OP_NOP,\
  &&LBL_OP_CALLG2 - &&LBL_OP_NOP,\
  &&LBL_OP_CALL - &&LBL_OP_NOP,\
  &&LBL_OP_CALLG - &&LBL_OP_NOP,\
  &&LBL_OP_FCALL - &&LBL_OP_NOP,\
  &&LBL_OP_MCALL - &&LBL_OP_NOP,\
  &&LBL_OP_POP - &&LBL_OP_NOP,\
  &&LBL_OP_POP1 - &&LBL_OP_NOP,\
  &&LBL_OP_POPREQ - &&LBL_OP_NOP,\
  &&LBL_OP_POPOPT - &&LBL_OP_NOP,\
  &&LBL_OP_NOMORE - &&LBL_OP_NOP,\
  &&LBL_OP_POPREST - &&LBL_OP_NOP,\
  &&LBL_OP_PUSHKEYS - &&LBL_OP_NOP,\
  &&LBL_OP_EXIT - &&LBL_OP_NOP,\
  &&LBL_OP_FLET - &&LBL_OP_NOP,\
  &&LBL_OP_LABELS - &&LBL_OP_NOP,\
  &&LBL_OP_LFUNCTION - &&LBL_OP_NOP,\
  &&LBL_OP_FUNCTION - &&LBL_OP_NOP,\
  &&LBL_OP_CLOSE - &&LBL_OP_NOP,\
  &&LBL_OP_GO - &&LBL_OP_NOP,\
  &&LBL_OP_RETURN - &&LBL_OP_NOP,\
  &&LBL_OP_THROW - &&LBL_OP_NOP,\
  &&LBL_OP_JMP - &&LBL_OP_NOP,\
  &&LBL_OP_JNIL - &&LBL_OP_NOP,\
  &&LBL_OP_JT - &&LBL_OP_NOP,\
  &&LBL_OP_JEQL - &&LBL_OP_NOP,\
  &&LBL_OP_JNEQL - &&LBL_OP_NOP,\
  &&LBL_OP_UNBIND - &&LBL_OP_NOP,\
  &&LBL_OP_UNBINDS - &&LBL_OP_NOP,\
  &&LBL_OP_BIND - &&LBL_OP_NOP,\
  &&LBL_OP_PBIND - &&LBL_OP_NOP,\
  &&LBL_OP_VBIND - &&LBL_OP_NOP,\
  &&LBL_OP_BINDS - &&LBL_OP_NOP,\
  &&LBL_OP_PBINDS - &&LBL_OP_NOP,\
  &&LBL_OP_VBINDS - &&LBL_OP_NOP,\
  &&LBL_OP_SETQ - &&LBL_OP_NOP,\
  &&LBL_OP_SETQS - &&LBL_OP_NOP,\
  &&LBL_OP_PSETQ - &&LBL_OP_NOP,\
  &&LBL_OP_PSETQS - &&LBL_OP_NOP,\
  &&LBL_OP_VSETQ - &&LBL_OP_NOP,\
  &&LBL_OP_VSETQS - &&LBL_OP_NOP,\
  &&LBL_OP_BLOCK - &&LBL_OP_NOP,\
  &&LBL_OP_DO - &&LBL_OP_NOP,\
  &&LBL_OP_CATCH - &&LBL_OP_NOP,\
  &&LBL_OP_FRAME - &&LBL_OP_NOP,\
  &&LBL_OP_TAGBODY - &&LBL_OP_NOP,\
  &&LBL_OP_EXIT_TAGBODY - &&LBL_OP_NOP,\
  &&LBL_OP_EXIT_FRAME - &&LBL_OP_NOP,\
  &&LBL_OP_PROTECT - &&LBL_OP_NOP,\
  &&LBL_OP_PROTECT_NORMAL - &&LBL_OP_NOP,\
  &&LBL_OP_PROTECT_EXIT - &&LBL_OP_NOP,\
  &&LBL_OP_PROGV - &&LBL_OP_NOP,\
  &&LBL_OP_EXIT_PROGV - &&LBL_OP_NOP,\
  &&LBL_OP_PUSHVALUES - &&LBL_OP_NOP,\
  &&LBL_OP_POPVALUES - &&LBL_OP_NOP,\
  &&LBL_OP_PUSHMOREVALUES - &&LBL_OP_NOP,\
  &&LBL_OP_VALUES - &&LBL_OP_NOP,\
  &&LBL_OP_VALUEREG0 - &&LBL_OP_NOP,\
  &&LBL_OP_NTHVAL - &&LBL_OP_NOP,\
  &&LBL_OP_NIL - &&LBL_OP_NOP,\
  &&LBL_OP_NOT - &&LBL_OP_NOP,\
  &&LBL_OP_PUSHNIL - &&LBL_OP_NOP,\
  &&LBL_OP_CSET - &&LBL_OP_NOP,\
  &&LBL_OP_STEPIN - &&LBL_OP_NOP,\
  &&LBL_OP_STEPCALL - &&LBL_OP_NOP,\
  &&LBL_OP_STEPOUT - &&LBL_OP_NOP };
#endif
