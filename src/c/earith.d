/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    earith.c -- Support for bignum arithmetic.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/*

	EXTENDED_MUL and EXTENDED_DIV perform 32 bit multiplication and
	division, respectively.

	EXTENDED_MUL(D,Q,R,HP,LP) 
	calculates D*Q+R and saves the result into the locations HP and LP.
	D, Q, and R are 32 bit non-negative integers and HP and LP are
	word addresses.  The word at LP will contain the lower 31 (not 32)
	bits of the result and its most significant bit is set 0. The word
	at HP will contain the rest of the result and its MSB is also set 0.

	EXTENDED_DIV(D,H,L,QP,RP)
	divides [H:L] by D and saves the quotient and the remainder into
	the locations QP and RP, respectively.  D, H, L are 32 bit non-negative
	integers and QP and RP are word addresses.  Here, [H:L] means the
	64 bit integer (imaginary) represented by H and L as follows.

	  63 62                  31 30                 0
	  |0|0|<lower 31 bits of H>|<lower 31 bits of L>|

	Although [H:L] is 64 bits, you can assume that the quotient is always
	represented as 32 bit non-negative integer.
*/

#include <ecl/ecl.h>

#ifdef CONVEX

static void
extended_mul(int d, int q, int r, int *hp, int *lp)
{
	long long int ld, lq, lr, z;
        int zh, zl;

        ld = d;
        lq = q;
        lr = r;
        z = ld*lq+lr;
        zl = (z & 0x000000007fffffffLL);
        zh = (z >> 31LL);
        *hp = zh;
        *lp = zl;
}

static void
extended_div(int d, int h, int l, int *qp, int *rp)
{
	long long int lh, ld, ll;

	ld = d;
	lh = h;
	ll = l;
	lh = (lh << 31LL);
	lh = (lh | ll);
	*qp = (lh/ld);
	*rp = (lh%ld);
      }
#endif /* CONVEX */

#ifdef i386

static void
extended_mul(int d, int q, int r, int *hp, int *lp)
{ asm("pushl    %ecx");
  asm("movl	8(%ebp),%eax");
  asm("mull	12(%ebp)");
  asm("addl	16(%ebp),%eax");
  asm("adcl	$0,%edx");
  asm("shll	$1,%edx");
  asm("btrl	$31,%eax");
  asm("adcl	$0,%edx");
  asm("movl	20(%ebp),%ecx");
  asm("movl	%edx, (%ecx)");
  asm("movl	24(%ebp), %ecx");
  asm("movl	%eax, (%ecx)");
  asm("popl	%ecx");
}

static void
extended_div(int d, int h, int l, int *qp, int *rp)
{ 
  asm("pushl	%ebx");
  asm("movl	12(%ebp),%edx");
  asm("movl	16(%ebp),%eax");
  asm("btl	$0,%edx");
  asm("jae	1f");
  asm("btsl	$31,%eax");
  asm("1: shrl	$1,%edx");
  asm("idivl	8(%ebp)");
  asm("movl	20(%ebp),%ebx");
  asm("movl	%eax,(%ebx)");
  asm("movl	24(%ebp),%ebx");
  asm("movl	%edx,(%ebx)");
  asm("popl	%ebx");
}
#endif /* i386 */

#ifdef IBMRT

static void
extended_mul(int d, int q, int r, int *hp, int *lp)
{
  /* d=L750+20, q=L750+24, etc. */
  asm(" get r0,L750+20(r13)");	/* get an argument */
  asm(" mts r10,r0");		/* put in MQ */
  asm(" get r2,L750+24(r13)");	/* get the other argument */
  asm(" s r0,r0");		/* zero partial product. set carry to 1. */
  asm(" m r0,r2
	 m r0,r2
	 m r0,r2
	 m r0,r2
	 m r0,r2
	 m r0,r2
	 m r0,r2
	 m r0,r2
	 m r0,r2
	 m r0,r2
	 m r0,r2
	 m r0,r2
	 m r0,r2
	 m r0,r2
	 m r0,r2
	 m r0,r2");
  /* Now (r0)//mq has the 64 bit product; overflow is ignored. */
  asm(" mfs r10,r2");		/* set r2 = low order word of result
				 * so product is in (r0)//(r2).
				 */
  /*
   * Force product into two single precision words.
   */

  asm(" get r3,$1f
	 sli r0,1
	 ar2,r2
	 bnc0r r3");      /* branch if carry = 0 */
  asm(" oil r0,r0,1
	1:
	 sri r2,1");
  /* Now add in the third argument. */
  asm(" get r4,$2f
	 get r3,L750+28(r13)
	 a r2,r3

	 bnmr r4");      /* branch if not minus */
  asm(" clrbu r2,0
	 lis r3,1
	 a r0,r3
	2:

	 get r3,L750+32(r13)
	 put r0,0(r3)
	 get r3,L750+36(r13)
	 put r2,0(r3)
	 ");
}

static void
extended_div(int d, int h, int l, int *qp, int *rp)
{
  /* d=L754+20, h=L754+24, etc. */
  /* Move arguments into registers. */
  asm(" get r0,L754+28(r13)");	/* Low order word of dividend. */
  asm(" get r2,L754+24(r13)");	/* High order word of dividend. */
  asm(" mttbil r2,15
	 mftbiu r0,0
	 sri r2,1
	 mts r10,r0
	 get r3,L754+20(r13)")		/* Divisor. */
    /* Perform 32 steps of division. */
    asm(" d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3
	 d r2,r3");
  /* Now MQ has the quotient, R2 the remainder, and R3 is
   * the unchanged divisor. */
  asm(" mttbiu r2,0");		/* Do add-back if necessary. */
  asm(" jntb 1f
	 a r2,r3
	1:
	 mfs r10,r0
	 c r2,r3");		/* Remainder : divisor. */
  asm(" jne 2f
	 inc r0,1
	 x r2,r2
	2:");
  /* Now r0 has the quotient and r2 has the remainder. */
  asm(" get r3,L754+32(r13)");	/* Quotient address. */
  asm(" put r0,0(r3)");
  asm(" get r3,L754+36(r13)");	/* Remainder address. */
  asm(" put r2,0(r3)");
}

#endif /* IBMRT */

#if defined(NEWS) || defined(MAC)

static void
extended_mul(int d, int q, int r, int *hp, int *lp)
{
	asm("	move.l	d2,-(sp)
		clr.l	d2
		move.l	(8,fp),d0
		mulu.l	(12,fp),d1:d0
		add.l	(16,fp),d0
		addx.l	d2,d1
		lsl.l	#1,d0
		roxl.l	#1,d1
		lsr.l	#1,d0
		move.l	(20,fp),a0
		move.l	d1,(a0)
		move.l	(24,a6),a0
		move.l	d0,(a0)");
}

static void
extended_div(int d, int h, int l, int *qp, int *rp)
{
	asm("movem.l	(12,fp),#0x303
	lsl.l	#1,d1
	lsr.l	#1,d0
	roxr.l	#1,d1
	divu.l	(8,fp),d0:d1
	move.l	d0,(a1)
	move.l	d1,(a0)
	");
}

#endif /* NEWS || MAC */

#ifdef __mips

 /*  earith.s for MIPS R2000 processor
      by Doug Katzman
      version 2.1.d dated 7/13/89 15:31 EDT */

/* Register names:
#define v0	$2	return value
#define v1	$3
#define a0	$4	argument registers
#define a1	$5
#define a2	$6
#define a3	$7
#define t7	$15
*/

static void
extended_mul(unsigned int d, unsigned int q, unsigned int r, unsigned int *hp,
	     unsigned int *lp)
{
  asm("mult	$4, $5");	/* [hi:lo] = d * q */
  asm("mfhi	$5");		/* a1 = hi	   */
  asm("sll	$5, 1");
  asm("mflo	$4");
  asm("srl	$15, $4, 31");
  asm("and	$4, 0x7fffffff");
  asm("or	$5, $15");
  asm("addu	$4, $6");	/* [a1:a0] += r */
  asm("srl	$15, $4, 31");
  asm("and	$4, 0x7fffffff");
  asm("addu	$5, $15");
  asm("sw	$5, 0($7)"); 	/* *hp = a1 */
#ifdef __GNUC__
  asm("lw	$7, %0" :: "g" (lp));
#else
  asm("lw	$7, 16($sp)");	/* fetch fifth actual argument from stack */
#endif
  asm("sw	$4, 0($7)"); 	/* *lp = a0 */
}

static void
extended_div(unsigned int d, unsigned int h, unsigned int l, unsigned int *qp,
	     unsigned int *rp)
{

  asm("sll	$6, 1");
  asm("li	$2, 31");	/* v0 holds number of shifts */
  asm("loop:
       srl	$15, $6, 31");
  asm("sll	$5, 1");
  asm("or	$5, $15");
  asm("sll	$6, 1");
  asm("subu	$15, $5, $4");	/* t = h - d */
  asm("bltz	$15, underflow");
  asm("move	$5, $15");
  asm("or	$6, 1");
  asm("underflow:
       subu	$2, 1");
  asm("bnez	$2, loop");
  asm("sw	$6, 0($7)");	/* *qp = l */
#ifdef __GNUC__
  asm("lw	$7, %0" :: "g" (rp));
#else
  asm("lw	$7, 16($sp)");	/* fetch fifth actual argument from stack */
#endif
  asm("sw	$5, 0($7)");	/* *rp = h */
}
#endif /* __mips */

#if defined(sun3) || (defined __NeXT)

static void
extended_mul(int d, int q, int r, int *hp, int *lp)
{
	asm("
		movl	d2,a7@-
		clrl	d2
		movl	a6@(8),d0
		mulul	a6@(12),d1:d0
		addl	a6@(16),d0
		addxl	d2,d1
		lsll	#1,d0
		roxll	#1,d1
		lsrl	#1,d0
		movl	a6@(20),a0
		movl	d1,a0@
		movl	a6@(24),a0
		movl	d0,a0@
		movl	a7@+,d2
		");
}

static void
extended_div(int d, int h, int l, int *qp, int *rp)
{
	asm("moveml	a6@(12),#0x303
	lsll	#1,d1
	lsrl	#1,d0
	roxrl	#1,d1
	divul	a6@(8),d0:d1
	movl	d0,a1@
	movl	d1,a0@
	");
}

#endif /* sun3 */

/* Possible assembler version: 
#ifdef sparc
_extended_mul:
!#PROLOGUE# 0
!#PROLOGUE# 1
	save	%sp,-96,%sp
	mov	%i0,%o0
	call	.umul,2
	mov	%i1,%o1
	addcc	%o0,%i2,%i0
	addx    %o1,0,%o1
	sll	%o1,1,%o1
	tst	%i0
	bge	L77003
	sethi	%hi(0x7fffffff),%o3
	or	%o3,%lo(0x7fffffff),%o3	! [internal]
	and	%i0,%o3,%i0
	inc	%o1
L77003:
	st	%i0,[%i4]
	st	%o1,[%i3]
	ret
	restore	%g0,0,%o0

#endif sparc
*/

#if defined(sparc) || defined(APOLLO) || defined(hpux) || defined(UNIGRAPH)n

/* for the time being use the C version:*/

static void
extended_mul(unsigned int d, unsigned int q, unsigned int r, unsigned int *hp,
	     unsigned int *lp)
{
  register unsigned short dlo = d & 0xffff,
  	dhi = d >> 16,
	qlo = q & 0xffff,
	qhi = q >> 16;
  unsigned int d0 = dhi * qlo + dlo * qhi,
  	d1 = dhi * qhi,
  	d2 = dlo * qlo;

  d1 = (d1 << 1) + (d0 >> 15);	/* add 17 MSB of d0 */
  d1 += d2 >> 31;		/* add MSB of d2 */
  d2 &= 0x7fffffff;		/* clear MSB of d2 */
  d2 += (d0 & 0x7fff) << 16;	/* add 15 LSB of d0: no overflow occurs */
  d1 += d2 >> 31;		/* add MSB of d2 */
  d2 &= 0x7fffffff;		/* clear MSB of d2 */
  d2 += r;
  d1 += d2 >> 31;		/* add MSB of d2 */
  d2 &= 0x7fffffff;		/* clear MSB of d2 */

  *hp = d1;
  *lp = d2;
}

static void
extended_div(unsigned int d, unsigned int h, unsigned int l, unsigned int *qp,
	     unsigned int *rp)
{
  int i;
  int borrow;

  l = (l << 1) | 1;

  for (i = 31; i >= 0;) { 

    if (h >= d) {
      h -= d;
      borrow = 0;
    }
    else
      borrow = 1;

    if (i--)
	h = (h << 1) | ((unsigned)l >> 31);

    l = (l << 1) | borrow;

  }

  l = - l - 1;

  *qp = l;
  *rp = h;
}

#endif /* sparc */

#ifdef vax

static void
extended_mul(int d, int q, int r, int *hp, int *lp)
{
	asm("	emul	4(ap),8(ap),12(ap),r0");
	asm("	ashq	$1,r0,r0");
	asm("	rotl	$-1,r0,r0");
	asm("	movl	r0,*20(ap)");
	asm("	movl	r1,*16(ap)");
}

static void
extended_div(int d, int h, int l, int *qp, int *rp)
{
	asm("	clrl	r0");
	asm("	movl	8(ap),r1");
	asm("	ashq	$-1,r0,r0");
	asm("	addl2	12(ap),r0");
	asm("	ediv	4(ap),r0,*16(ap),*20(ap)");
}
#endif /* vax */
