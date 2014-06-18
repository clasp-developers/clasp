/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    cs.h -- C stack manipulation.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


/*
 *----------------------------------------------------------------------
 *	Low level stack manipulation macros
 *----------------------------------------------------------------------
 */

#define asm __asm__

/*
 *----------------------------------------------------------------------
 *	Stack of predefined size
 *----------------------------------------------------------------------
 */

#define cl_nextarg(arg)		va_arg((arg),cl_object)

#define CSTACK(size)     	register cl_object *_stack_top; \
  				cl_object _stack_bot[size]; \
  				_stack_top = _stack_bot /*  __GNUC__ */
#define CPUSH(val)		*_stack_top++ = (val);

#define CSTACK_BOT		_stack_bot
#define CSTACK_TOP		_stack_top

/*---------------------------------------------------------------------- */

#if defined(__i386__) && 0
/* Stack usage:

   sp	|  lclm  |
	|  ...   |
   	|  lcl1  |
   bp	|  obp   |
        |  ret?  |
        |  arg1  |
        |  ...   |
   osp	|  argn  |
	|  ...   |
   obp  |        |

Caller pushes args and pops them after return. Result in ax.
*/

#ifdef __GO32__ /* DJGPP */
typedef char *  caddr_t;
#endif

#define GET_CURRENT_FRAME(frame)	asm("movl (%%ebp),%0" : "=r" (frame))
#define FRAME_CHAIN(frame)		((char **)frame)[0]
#define FRAME_SAVED_PC(frame)		((char **)frame)[1]

#define PC_INDEX 8

#if 0
#define TRANSFER_CALL(fun)	asm("leave"); goto *(fun) /* __GNUC__ */
#endif

#define CCALL(narg,fun)	  	({register cl_object eax asm("%eax"); \
				    asm("push %0" :: "g" (narg)); \
				    asm("call %0" :: "g" (fun) : "eax"); \
 				    asm("addl $4, %%esp" :: ); eax;})

#if !defined(__linux__) && !defined(__FreeBSD__)
#define ARGCALL CCALL
#define ARGSTACK(size)	CSTACK(size)
#endif

#endif

/*---------------------------------------------------------------------- */

#ifdef __mips

/* stack usage:

   sp	|           |
	|   ...     |
        | save $31  | \ return pointer
        | save regs |  > frame offset
	|  locals   | /
   fp   |   arg1    |  first arg of this function
	|   ...     |
   	|   argn    |
	|   ...     |
        |           |

*/
/* Unfortunately MIPS stacks do not have enough information to chain through
   them.  FP is virtual, has no allocated register. We might reconstruct
   framesize and frameoffset from first two instructions of procedure:

     subu $sp, framesize
     sw $ra, framesize+frameoffset($sp)

   However we have no way to find the entry to a function given a PC.
   GDB uses symbol information from the function to determine FP.
*/

#define ARGSTACK(size)     	register cl_object *_stack_top; \
  				cl_object _stack_bot[size+1]; \
  				_stack_top = _stack_bot /*  __GNUC__ */

#endif

/*---------------------------------------------------------------------- */

#if defined(sparc) && 0
   
/*
#define JB_SP 1
#define JB_PC 2
#define JB_FP 3
#define JB_NPC 4
#define JB_PSR 5
*/

/*
 * Got this info from gdb (config/sparc/tm-sparc.h and sparc-tdep.c)
 */

#define GET_CURRENT_FRAME(frame)	asm("mov %%fp,%0" : "=r" (frame))
#define FRAME_CHAIN(frame)	((caddr_t *)frame)[14]
#define FRAME_SAVED_PC(frame)	((caddr_t *)frame)[15]


/* GCC reserves 6 words for saving registers o0-05, but never does that */

#define ARGSTACK(size)     	register volatile cl_object *_stack_top; \
  				cl_object _stack_bot[size+1]; \
  				_stack_top = _stack_bot-6 /*  __GNUC__ */

#define ARGCALL(narg, fun)  ({ register cl_object _res asm("%o0"); \
			     asm("mov %0,%%o0" :: "r" (narg) : "%o0"); \
			     asm("ld [%%sp+72],%%o1" ::: "%o1"); \
			     asm("ld [%%sp+76],%%o2" ::: "%o2"); \
			     asm("ld [%%sp+80],%%o3" ::: "%o3"); \
			     asm("ld [%%sp+84],%%o4" ::: "%o4"); \
			     asm("ld [%%sp+88],%%o5" ::: "%o5"); \
			     asm("call %0,0" : : "r" (fun) ); \
       			     asm("nop"); _res;})

#endif

/*---------------------------------------------------------------------- */

#if defined(vax) && 0
#define PC_INDEX 0
#define TRANSFER(buf, addr)	buf[PC_INDEX] = (int)addr+((((int *)addr)[0] >> 19) & 4)+4; \
  				ecl_longjmp(buf)
#define TRANSFER_CALL(fun)	REG = (cl_object)fun; \
				asm("	ashl	$-19,(r11),r0"); \
				asm("	bicl2	$-5,r0"); \
				asm("	addl2	r11,r0"); \
				asm("	addl2	$4,r0"); \
				asm("	jmp	(r0)")
#define CSTACK(size)		register cl_object REG
#define CPUSH(val)		(REG = (cl_object)(val), \
				asm("	pushl	r11"))
#define CPOP			(asm("	movl	(sp)+,r11"), REG)

/* Reverse arguments on the stack, push narg, and then call.
   narg (r11), up (r9), dn (r8)
  for (dn = sp, up = sp+narg-1; dn < up; ) {
    tmp = *dn; *dn++ = *up; *up-- = tmp;
  }
*/
#define CCALL(narg, fun)		(REG = (cl_object)(narg), \
				 asm("	movl	sp,r8"), \
				 asm("	ashl	$2,r11,r0"), \
				 asm("	addl3	r0,sp,r9"), \
				 asm("	subl2	$4,r9"), \
				 asm("1:	cmpl	r8,r9"), \
				 asm("	jgeq	2f"), \
				 asm("	movl	(r8),r7"), \
				 asm("	movl	(r9),(r8)+"), \
				 asm("	movl	r7,(r9)"), \
				 asm("	subl2	$4,r9"), \
				 asm("	jbr 	1b"), \
				 asm("2:	pushl	r11"), \
				 asm("	addl3	$1,r11,r0"), \
				 REG = (cl_object)(fun), \
				 asm("	calls	r0,(r11)"), \
				 asm("	movl	r0,r11"), (int)REG)
#endif

/*---------------------------------------------------------------------- */
