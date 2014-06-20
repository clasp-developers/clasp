/* mpq_div -- divide two rational numbers.

Copyright 1991, 1994, 1995, 1996, 2000, 2001 Free Software Foundation, Inc.

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

#include "gmp.h"
#include "gmp-impl.h"


void
mpq_div (mpq_ptr quot, mpq_srcptr op1, mpq_srcptr op2)
{
  mpz_t gcd1, gcd2;
  mpz_t tmp1, tmp2;
  mpz_t numtmp;
  mp_size_t op1_num_size;
  mp_size_t op1_den_size;
  mp_size_t op2_num_size;
  mp_size_t op2_den_size;
  mp_size_t alloc;
  TMP_DECL;

  op1_num_size = ABS (op1->_mp_num._mp_size);
  op1_den_size =      op1->_mp_den._mp_size;
  op2_num_size = ABS (op2->_mp_num._mp_size);
  op2_den_size =      op2->_mp_den._mp_size;

  if (op2_num_size == 0)
    DIVIDE_BY_ZERO;

  if (op1_num_size == 0)
    {
      /* We special case this to simplify allocation logic; gcd(0,x) = x
	 is a singular case for the allocations.  */
      quot->_mp_num._mp_size = 0;
      quot->_mp_den._mp_d[0] = 1;
      quot->_mp_den._mp_size = 1;
      return;
    }

  TMP_MARK;

  alloc = MIN (op1_num_size, op2_num_size);
  MPZ_TMP_INIT (gcd1, alloc);

  alloc = MIN (op1_den_size, op2_den_size);
  MPZ_TMP_INIT (gcd2, alloc);

  alloc = MAX (op1_num_size, op2_num_size);
  MPZ_TMP_INIT (tmp1, alloc);

  alloc = MAX (op1_den_size, op2_den_size);
  MPZ_TMP_INIT (tmp2, alloc);

  alloc = op1_num_size + op2_den_size;
  MPZ_TMP_INIT (numtmp, alloc);

  /* QUOT might be identical to either operand, so don't store the result there
     until we are finished with the input operands.  We can overwrite the
     numerator of QUOT when we are finished with the numerators of OP1 and
     OP2.  */

  mpz_gcd (gcd1, &(op1->_mp_num), &(op2->_mp_num));
  mpz_gcd (gcd2, &(op2->_mp_den), &(op1->_mp_den));

  mpz_divexact_gcd (tmp1, &(op1->_mp_num), gcd1);
  mpz_divexact_gcd (tmp2, &(op2->_mp_den), gcd2);

  mpz_mul (numtmp, tmp1, tmp2);

  mpz_divexact_gcd (tmp1, &(op2->_mp_num), gcd1);
  mpz_divexact_gcd (tmp2, &(op1->_mp_den), gcd2);

  mpz_mul (&(quot->_mp_den), tmp1, tmp2);

  /* We needed to go via NUMTMP to take care of QUOT being the same as OP2.
     Now move NUMTMP to QUOT->_mp_num.  */
  mpz_set (&(quot->_mp_num), numtmp);

  /* Keep the denominator positive.  */
  if (quot->_mp_den._mp_size < 0)
    {
      quot->_mp_den._mp_size = -quot->_mp_den._mp_size;
      quot->_mp_num._mp_size = -quot->_mp_num._mp_size;
    }

  TMP_FREE;
}
