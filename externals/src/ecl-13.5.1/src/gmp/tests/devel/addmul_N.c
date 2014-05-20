/*
Copyright 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004 Free Software
Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
MA 02110-1301, USA.
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

#if defined (USG) || defined (__SVR4) || defined (_UNICOS) || defined (__hpux)
#include <time.h>

int
cputime ()
{
  if (CLOCKS_PER_SEC < 100000)
    return clock () * 1000 / CLOCKS_PER_SEC;
  return clock () / (CLOCKS_PER_SEC / 1000);
}
#else
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>

int
cputime ()
{
  struct rusage rus;

  getrusage (0, &rus);
  return rus.ru_utime.tv_sec * 1000 + rus.ru_utime.tv_usec / 1000;
}
#endif

static void print_posneg (mp_limb_t);
static void mpn_print (mp_ptr, mp_size_t);

#define LXW ((int) (2 * sizeof (mp_limb_t)))
#define M * 1000000

#ifndef CLOCK
#error "Don't know CLOCK of your machine"
#endif

#ifndef OPS
#define OPS (CLOCK/5)
#endif
#ifndef SIZE
#define SIZE 496
#endif
#ifndef TIMES
#define TIMES OPS/(SIZE+1)
#endif

#if N == 2
#define mpn_addmul_N mpn_addmul_2
#elif N == 3
#define mpn_addmul_N mpn_addmul_3
#elif N == 4
#define mpn_addmul_N mpn_addmul_4
#elif N == 5
#define mpn_addmul_N mpn_addmul_5
#elif N == 6
#define mpn_addmul_N mpn_addmul_6
#elif N == 7
#define mpn_addmul_N mpn_addmul_7
#elif N == 8
#define mpn_addmul_N mpn_addmul_8
#endif

mp_limb_t
refmpn_addmul_N (mp_ptr rp, mp_srcptr up, mp_size_t n, mp_srcptr vp)
{
  int i;
  for (i = 1; i < N; i++)
    {
      rp[n] = mpn_addmul_1 (rp, up, n, *vp);
      rp++;
      vp++;
    }
  return mpn_addmul_1 (rp, up, n, *vp);
}

int
main (int argc, char **argv)
{
  mp_limb_t up[SIZE];
  mp_limb_t ref[SIZE + N - 1];
  mp_limb_t mem[SIZE + N + 1];
  mp_ptr rp = mem + 1;
  mp_limb_t vp[N];
  mp_limb_t cy_ref, cy_try;
  int i;
  long t0, t;
  unsigned test;
  mp_size_t size;
  double cyc;
  unsigned ntests;

  ntests = ~(unsigned) 0;
  if (argc == 2)
    ntests = strtol (argv[1], 0, 0);

  for (test = 1; test <= ntests; test++)
    {
#if TIMES == 1 && ! defined (PRINT)
      if (test % (CLOCK / SIZE / 1000) == 0)
	{
	  printf ("\r%u", test);
	  fflush (stdout);
	}
#endif

#ifdef RANDOM
      size = random () % SIZE + 1;
#else
      size = SIZE;
#endif

      rp[size + N - 1] = 0x12345678;
      rp[-1] = 0x87654321;

      mpn_random (vp, N);

#if TIMES != 1			/* run timing tests unless asked not to */
      mpn_random (up, size);
      mpn_random (rp, size + N - 1);

      MPN_COPY (ref, rp, size + N - 1);
      t0 = cputime();
      for (i = 0; i < TIMES; i++)
	mpn_addmul_N (ref, up, size, vp);
      t = cputime() - t0;
      cyc = ((double) t * CLOCK) / (TIMES * size * 1000.0) / N;
      printf ("mpn_addmul_N:    %5ldms (%.3f cycles/limb) [%.2f Gb/s]\n",
	      t, cyc, CLOCK/cyc*BITS_PER_MP_LIMB*BITS_PER_MP_LIMB/1e9);
#endif

#ifdef ZEROu
      MPN_ZERO (up, size);
#else
      mpn_random2 (up, size);
#endif
      mpn_random2 (vp, N);
#ifdef ZERO
      MPN_ZERO (rp, size + N - 1);
#else
      mpn_random2 (rp, size + N - 1);
#endif

#if defined (PRINT) || defined (PRINTV)
      printf ("vp=");
      mpn_print (vp, N);
#endif
#ifdef PRINT
      printf ("%*s ", 3 + N * LXW, "");
      mpn_print (rp, size);
      printf ("%*s ", 3 + N * LXW, "");
      mpn_print (up, size);
#endif

      MPN_COPY (ref, rp, size + N - 1);
      cy_ref = refmpn_addmul_N (ref, up, size, vp);
      cy_try = mpn_addmul_N (rp, up, size, vp);

#ifdef PRINT
      printf ("%*lX ", LXW, cy_ref);
      mpn_print (ref, size + N - 1);
      printf ("%*lX ", LXW, cy_try);
      mpn_print (rp, size + N - 1);
#endif

#ifndef NOCHECK
      if (cy_ref != cy_try || mpn_cmp (ref, rp, size + N - 1) != 0
	  || rp[size + N - 1] != 0x12345678 || rp[-1] != 0x87654321)
	{
	  printf ("\n        ref%*s try%*s diff\n", LXW - 3, "", 2 * LXW - 6, "");
	  for (i = 0; i < size + N - 1; i++)
	    {
	      printf ("%6d: ", i);
	      printf ("%0*llX ", LXW, (unsigned long long) ref[i]);
	      printf ("%0*llX ", LXW, (unsigned long long) rp[i]);
	      print_posneg (rp[i] - ref[i]);
	      printf ("\n");
	    }
	  printf ("retval: ");
	  printf ("%0*llX ", LXW, (unsigned long long) cy_ref);
	  printf ("%0*llX ", LXW, (unsigned long long) cy_try);
	  print_posneg (cy_try - cy_ref);
	  printf ("\n");
	  if (rp[-1] != 0x87654321)
	    printf ("clobbered at low end\n");
	  if (rp[size + N - 1] != 0x12345678)
	    printf ("clobbered at high end\n");
	  printf ("TEST NUMBER %u\n", test);
	  abort();
	}
#endif
    }
  exit (0);
}

static void
print_posneg (mp_limb_t d)
{
  char buf[LXW + 2];
  if (d == 0)
    printf (" %*X", LXW, 0);
  else if (-d < d)
    {
      sprintf (buf, "%llX", (unsigned long long) -d);
      printf ("%*s-%s", LXW - (int) strlen (buf), "", buf);
    }
  else
    {
      sprintf (buf, "%llX", (unsigned long long) d);
      printf ("%*s+%s", LXW - (int) strlen (buf), "", buf);
    }
}

static void
mpn_print (mp_ptr p, mp_size_t size)
{
  mp_size_t i;

  for (i = size - 1; i >= 0; i--)
    {
#ifdef _LONG_LONG_LIMB
      printf ("%0*lX%0*lX", (int) (sizeof(mp_limb_t)),
	      (unsigned long) (p[i] >> (BITS_PER_MP_LIMB/2)),
              (int) (sizeof(mp_limb_t)), (unsigned long) (p[i]));
#else
      printf ("%0*lX", LXW, p[i]);
#endif
#ifdef SPACE
      if (i != 0)
	printf (" ");
#endif
    }
  puts ("");
}
