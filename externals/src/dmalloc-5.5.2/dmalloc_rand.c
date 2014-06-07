/*
 * Internal random number generator
 *
 * This file is part of the dmalloc package.
 *
 * Permission to use, copy, modify, and distribute this software for
 * any purpose and without fee is hereby granted, provided that the
 * above copyright notice and this permission notice appear in all
 * copies, and that the name of Gray Watson not be used in advertising
 * or publicity pertaining to distribution of the document or software
 * without specific, written prior permission.
 *
 * Gray Watson makes no representations about the suitability of the
 * software described herein for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * The author of dmalloc may be contacted via http://dmalloc.com/
 *
 * $Id: dmalloc_rand.c,v 1.3 2004/07/08 04:53:27 gray Exp $
 */

/*
 * Minimal Standard Pseudo-Random Number Generator
 *
 * Author: Fuat C. Baran, Columbia University, 1988
 *
 * Based on code in "Random Number Generators: Good Ones are Hard to
 * Find", by Stephen K. Park and Keith W. Miller in Communications of
 * the ACM, 31, 10 (Oct. 1988) pp. 1192-1201.
 *
 * Requirements: MAXINT must be 2 ^ 31 - 1 or larger
 *
 * Auto-seeding random number generator.  Just start to call random
 * and it will take care of seeding, etc.
 */

#include "dmalloc_rand.h"

#define MAGIC_A			16807		/* magic number */
#define MERSENNE_PRIME		2147483647UL	/* mersenne prime 2^31 -1 */
#define MAGIC_QUOTIENT		127773		/* M div A (M / A) */
#define MAGIC_REMAINDER		2836		/* M mod A (M % A) */

/* local variables */
static long	value = 0;			/* our random value */

/*
 * static void auto_seed
 *
 * DESCRIPTION:
 *
 * Automatically seed the random number generation algorithm.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * None.
 */
static	void	auto_seed(void)
{
  /* set the seed to a constant so we don't produce random addresses */
  value = 0xDEADBEEF;
}

/*
 * static void _dmalloc_srand
 *
 * DESCRIPTION:
 *
 * Seed the random number generator with the user argument.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * seed -> Value to seed the algorithm with.
 */
void	_dmalloc_srand(const long seed)
{
  value = seed;
}

/*
 * static long _dmalloc_rand
 *
 * DESCRIPTION:
 *
 * Get a pseudo-random number from the random algorithm.
 *
 * RETURNS:
 *
 * Random number.
 *
 * ARGUMENTS:
 *
 * None.
 */
long	_dmalloc_rand(void)
{
  if (value == 0) {
    auto_seed();
  }
  
  /* do the magic seed calculation */
  value = MAGIC_A * (value % MAGIC_QUOTIENT) -
    MAGIC_REMAINDER * (value / MAGIC_QUOTIENT);
  
  if (value <= 0) {
    value += MERSENNE_PRIME;
  }
  
  return value;
}
