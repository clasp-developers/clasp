/* SPARC v9 32-bit gmp-mparam.h -- Compiler/machine parameter header file.

Copyright 1991, 1993, 1994, 1999, 2000, 2001, 2002, 2004, 2009, 2010 Free
Software Foundation, Inc.

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

#define GMP_LIMB_BITS 32
#define BYTES_PER_MP_LIMB 4

#define DIVREM_1_NORM_THRESHOLD              3
#define DIVREM_1_UNNORM_THRESHOLD            5
#define MOD_1_NORM_THRESHOLD                 4
#define MOD_1_UNNORM_THRESHOLD               7
#define MOD_1N_TO_MOD_1_1_THRESHOLD      MP_SIZE_T_MAX  /* never */
#define MOD_1U_TO_MOD_1_1_THRESHOLD      MP_SIZE_T_MAX
#define MOD_1_1_TO_MOD_1_2_THRESHOLD     MP_SIZE_T_MAX
#define MOD_1_2_TO_MOD_1_4_THRESHOLD     MP_SIZE_T_MAX
#define PREINV_MOD_1_TO_MOD_1_THRESHOLD  MP_SIZE_T_MAX  /* never */
#define USE_PREINV_DIVREM_1                  1
#define DIVREM_2_THRESHOLD                   0  /* always */
#define DIVEXACT_1_THRESHOLD                 0  /* always */
#define BMOD_1_TO_MOD_1_THRESHOLD        MP_SIZE_T_MAX  /* never */

#define MUL_TOOM22_THRESHOLD                32
#define MUL_TOOM33_THRESHOLD                96
#define MUL_TOOM44_THRESHOLD               143
#define MUL_TOOM6H_THRESHOLD               216
#define MUL_TOOM8H_THRESHOLD               494

#define MUL_TOOM32_TO_TOOM43_THRESHOLD      96
#define MUL_TOOM32_TO_TOOM53_THRESHOLD     145
#define MUL_TOOM42_TO_TOOM53_THRESHOLD      97
#define MUL_TOOM42_TO_TOOM63_THRESHOLD      92

#define SQR_BASECASE_THRESHOLD              12
#define SQR_TOOM2_THRESHOLD                 62
#define SQR_TOOM3_THRESHOLD                103
#define SQR_TOOM4_THRESHOLD                274
#define SQR_TOOM6_THRESHOLD                274
#define SQR_TOOM8_THRESHOLD                542

#define MULMOD_BNM1_THRESHOLD               14
#define SQRMOD_BNM1_THRESHOLD               21

#define MUL_FFT_TABLE  { 272, 736, 1152, 3584, 10240, 24576, 98304, 917504, 0 }
#define MUL_FFT_MODF_THRESHOLD             248
#define MUL_FFT_THRESHOLD                 2112

#define SQR_FFT_TABLE  { 336, 800, 1408, 3584, 10240, 24576, 98304, 393216, 0 }
#define SQR_FFT_MODF_THRESHOLD             248
#define SQR_FFT_THRESHOLD                 2112

#define MULLO_BASECASE_THRESHOLD             0  /* always */
#define MULLO_DC_THRESHOLD                 106
#define MULLO_MUL_N_THRESHOLD             3493

#define DC_DIV_QR_THRESHOLD                123
#define DC_DIVAPPR_Q_THRESHOLD             396
#define DC_BDIV_QR_THRESHOLD               121
#define DC_BDIV_Q_THRESHOLD                280

#define INV_MULMOD_BNM1_THRESHOLD           62
#define INV_NEWTON_THRESHOLD               351
#define INV_APPR_THRESHOLD                 357

#define BINV_NEWTON_THRESHOLD              324
#define REDC_1_TO_REDC_N_THRESHOLD          78

#define MU_DIV_QR_THRESHOLD               1895
#define MU_DIVAPPR_Q_THRESHOLD            1895
#define MUPI_DIV_QR_THRESHOLD              122
#define MU_BDIV_QR_THRESHOLD               872
#define MU_BDIV_Q_THRESHOLD               2801

#define MATRIX22_STRASSEN_THRESHOLD         13
#define HGCD_THRESHOLD                     144
#define GCD_DC_THRESHOLD                   630
#define GCDEXT_DC_THRESHOLD                416
#define JACOBI_BASE_METHOD                   2

#define GET_STR_DC_THRESHOLD                 9
#define GET_STR_PRECOMPUTE_THRESHOLD        17
#define SET_STR_DC_THRESHOLD               537
#define SET_STR_PRECOMPUTE_THRESHOLD      1576
