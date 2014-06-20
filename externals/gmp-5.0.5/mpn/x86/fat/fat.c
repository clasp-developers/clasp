/* x86 fat binary initializers.

   THE FUNCTIONS AND VARIABLES IN THIS FILE ARE FOR INTERNAL USE ONLY.
   THEY'RE ALMOST CERTAIN TO BE SUBJECT TO INCOMPATIBLE CHANGES OR DISAPPEAR
   COMPLETELY IN FUTURE GNU MP RELEASES.

Copyright 2003, 2004, 2011 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library.  If not, see http://www.gnu.org/licenses/.  */

#include <stdio.h>    /* for printf */
#include <stdlib.h>   /* for getenv */
#include <string.h>

#include "gmp.h"
#include "gmp-impl.h"

/* Change this to "#define TRACE(x) x" for some traces. */
#define TRACE(x)

/* Change this to 1 to take the cpuid from GMP_CPU_TYPE env var. */
#define WANT_FAKE_CPUID  0


/* fat_entry.asm */
long __gmpn_cpuid __GMP_PROTO ((char dst[12], int id));
int  __gmpn_cpuid_available __GMP_PROTO ((void));


#if WANT_FAKE_CPUID
/* The "name"s in the table are values for the GMP_CPU_TYPE environment
   variable.  Anything can be used, but for now it's the canonical cpu types
   as per config.guess/config.sub.  */

#define __gmpn_cpuid            fake_cpuid
#define __gmpn_cpuid_available  fake_cpuid_available

#define MAKE_FMS(family, model)						\
  ((((family) & 0xf) << 8) + (((family) & 0xff0) << 20)			\
   + (((model) & 0xf) << 4) + (((model)  &  0xf0) << 12))

static struct {
  const char  *name;
  const char  vendor[13];
  unsigned    fms;
} fake_cpuid_table[] = {
  { "i386",       "" },
  { "i486",       "GenuineIntel", MAKE_FMS (4, 0) },
  { "pentium",    "GenuineIntel", MAKE_FMS (5, 0) },
  { "pentiummmx", "GenuineIntel", MAKE_FMS (5, 4) },
  { "pentiumpro", "GenuineIntel", MAKE_FMS (6, 0) },
  { "pentium2",   "GenuineIntel", MAKE_FMS (6, 2) },
  { "pentium3",   "GenuineIntel", MAKE_FMS (6, 7) },
  { "pentium4",   "GenuineIntel", MAKE_FMS (7, 0) },

  { "k5",         "AuthenticAMD", MAKE_FMS (5, 0) },
  { "k6",         "AuthenticAMD", MAKE_FMS (5, 3) },
  { "k62",        "AuthenticAMD", MAKE_FMS (5, 8) },
  { "k63",        "AuthenticAMD", MAKE_FMS (5, 9) },
  { "athlon",     "AuthenticAMD", MAKE_FMS (6, 0) },
  { "x86_64",     "AuthenticAMD", MAKE_FMS (15, 0) },

  { "viac3",      "CentaurHauls", MAKE_FMS (6, 0) },
  { "viac32",     "CentaurHauls", MAKE_FMS (6, 9) },
};

static int
fake_cpuid_lookup (void)
{
  char  *s;
  int   i;

  s = getenv ("GMP_CPU_TYPE");
  if (s == NULL)
    {
      printf ("Need GMP_CPU_TYPE environment variable for fake cpuid\n");
      abort ();
    }

  for (i = 0; i < numberof (fake_cpuid_table); i++)
    if (strcmp (s, fake_cpuid_table[i].name) == 0)
      return i;

  printf ("GMP_CPU_TYPE=%s unknown\n", s);
  abort ();
}

static int
fake_cpuid_available (void)
{
  return fake_cpuid_table[fake_cpuid_lookup()].vendor[0] != '\0';
}

static long
fake_cpuid (char dst[12], int id)
{
  int  i = fake_cpuid_lookup();

  switch (id) {
  case 0:
    memcpy (dst, fake_cpuid_table[i].vendor, 12);
    return 0;
  case 1:
    return fake_cpuid_table[i].fms;
  default:
    printf ("fake_cpuid(): oops, unknown id %d\n", id);
    abort ();
  }
}
#endif


typedef DECL_preinv_divrem_1 ((*preinv_divrem_1_t));
typedef DECL_preinv_mod_1    ((*preinv_mod_1_t));

struct cpuvec_t __gmpn_cpuvec = {
  __MPN(add_n_init),
  __MPN(addmul_1_init),
  __MPN(copyd_init),
  __MPN(copyi_init),
  __MPN(divexact_1_init),
  __MPN(divexact_by3c_init),
  __MPN(divrem_1_init),
  __MPN(gcd_1_init),
  __MPN(lshift_init),
  __MPN(mod_1_init),
  __MPN(mod_34lsub1_init),
  __MPN(modexact_1c_odd_init),
  __MPN(mul_1_init),
  __MPN(mul_basecase_init),
  __MPN(preinv_divrem_1_init),
  __MPN(preinv_mod_1_init),
  __MPN(rshift_init),
  __MPN(sqr_basecase_init),
  __MPN(sub_n_init),
  __MPN(submul_1_init),
  0
};


/* The following setups start with generic x86, then overwrite with
   specifics for a chip, and higher versions of that chip.

   The arrangement of the setups here will normally be the same as the $path
   selections in configure.in for the respective chips.

   This code is reentrant and thread safe.  We always calculate the same
   decided_cpuvec, so if two copies of the code are running it doesn't
   matter which completes first, both write the same to __gmpn_cpuvec.

   We need to go via decided_cpuvec because if one thread has completed
   __gmpn_cpuvec then it may be making use of the threshold values in that
   vector.  If another thread is still running __gmpn_cpuvec_init then we
   don't want it to write different values to those fields since some of the
   asm routines only operate correctly up to their own defined threshold,
   not an arbitrary value.  */

void
__gmpn_cpuvec_init (void)
{
  struct cpuvec_t  decided_cpuvec;

  TRACE (printf ("__gmpn_cpuvec_init:\n"));

  memset (&decided_cpuvec, '\0', sizeof (decided_cpuvec));

  CPUVEC_SETUP_x86;
  CPUVEC_SETUP_fat;

  if (! __gmpn_cpuid_available ())
    {
      TRACE (printf ("  80386, or early 80486 without cpuid\n"));
    }
  else
    {
      char vendor_string[13];
      char dummy_string[12];
      long fms;
      int family, model;

      __gmpn_cpuid (vendor_string, 0);
      vendor_string[12] = 0;

      fms = __gmpn_cpuid (dummy_string, 1);
      family = ((fms >> 8) & 0xf) + ((fms >> 20) & 0xff);
      model = ((fms >> 4) & 0xf) + ((fms >> 12) & 0xf0);

      if (strcmp (vendor_string, "GenuineIntel") == 0)
        {
          switch (family)
            {
            case 4:
              TRACE (printf ("  80486 with cpuid\n"));
              break;

            case 5:
              TRACE (printf ("  pentium\n"));
              CPUVEC_SETUP_pentium;
              if (model >= 4)
                {
                  TRACE (printf ("  pentiummmx\n"));
                  CPUVEC_SETUP_pentium_mmx;
                }
              break;

            case 6:
              TRACE (printf ("  p6\n"));
              CPUVEC_SETUP_p6;
              if (model >= 2)
                {
                  TRACE (printf ("  pentium2\n"));
                  CPUVEC_SETUP_p6_mmx;
                }
              if (model >= 7)
                {
                  TRACE (printf ("  pentium3\n"));
                  CPUVEC_SETUP_p6_p3mmx;
                }
              if (model >= 0xD || model == 9)
                {
                  TRACE (printf ("  p6 with sse2\n"));
                  CPUVEC_SETUP_p6_sse2;
                }
              break;

            case 15:
              TRACE (printf ("  pentium4\n"));
              CPUVEC_SETUP_pentium4;
              CPUVEC_SETUP_pentium4_mmx;
              CPUVEC_SETUP_pentium4_sse2;
              break;
            }
        }
      else if (strcmp (vendor_string, "AuthenticAMD") == 0)
        {
          switch (family)
            {
            case 5:
              if (model <= 3)
                {
                  TRACE (printf ("  k5\n"));
                }
              else
                {
                  TRACE (printf ("  k6\n"));
                  CPUVEC_SETUP_k6;
                  CPUVEC_SETUP_k6_mmx;
                  if (model >= 8)
                    {
                      TRACE (printf ("  k62\n"));
                      CPUVEC_SETUP_k6_k62mmx;
                    }
                  if (model >= 9)
                    {
                      TRACE (printf ("  k63\n"));
                    }
                }
              break;
            case 6:
              TRACE (printf ("  athlon\n"));
            athlon:
              CPUVEC_SETUP_k7;
              CPUVEC_SETUP_k7_mmx;
              break;
            case 15:
              TRACE (printf ("  x86_64\n"));
              goto athlon;
            }
        }
      else if (strcmp (vendor_string, "CentaurHauls") == 0)
        {
          switch (family)
            {
            case 6:
              TRACE (printf ("  viac3\n"));
              if (model >= 9)
                {
                  TRACE (printf ("  viac32\n"));
                }
              break;
            }
        }
      else if (strcmp (vendor_string, "CyrixInstead") == 0)
        {
          /* Should recognize Cyrix' processors too.  */
          TRACE (printf ("  cyrix something\n"));
        }
    }

  /* There's no x86 generic mpn_preinv_divrem_1 or mpn_preinv_mod_1.
     Instead default to the plain versions from whichever CPU we detected.
     The function arguments are compatible, no need for any glue code.  */
  if (decided_cpuvec.preinv_divrem_1 == NULL)
    decided_cpuvec.preinv_divrem_1 =(preinv_divrem_1_t)decided_cpuvec.divrem_1;
  if (decided_cpuvec.preinv_mod_1 == NULL)
    decided_cpuvec.preinv_mod_1    =(preinv_mod_1_t)   decided_cpuvec.mod_1;

  ASSERT_CPUVEC (decided_cpuvec);
  CPUVEC_INSTALL (decided_cpuvec);

  /* Set this once the threshold fields are ready.
     Use volatile to prevent it getting moved.  */
  ((volatile struct cpuvec_t *) &__gmpn_cpuvec)->initialized = 1;
}
