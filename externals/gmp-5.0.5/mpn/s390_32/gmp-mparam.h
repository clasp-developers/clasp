/* S/390-32 gmp-mparam.h -- Compiler/machine parameter header file.

Copyright 1991, 1993, 1994, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
2008, 2009, 2010, 2011 Free Software Foundation, Inc.

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

#define DIVREM_1_NORM_THRESHOLD              0  /* always */
#define DIVREM_1_UNNORM_THRESHOLD            5
#define MOD_1_NORM_THRESHOLD                 0  /* always */
#define MOD_1_UNNORM_THRESHOLD               3
#define MOD_1N_TO_MOD_1_1_THRESHOLD         13
#define MOD_1U_TO_MOD_1_1_THRESHOLD          6
#define MOD_1_1_TO_MOD_1_2_THRESHOLD         0  /* never mpn_mod_1_1p */
#define MOD_1_2_TO_MOD_1_4_THRESHOLD        35
#define PREINV_MOD_1_TO_MOD_1_THRESHOLD     21
#define USE_PREINV_DIVREM_1                  1
#define DIVREM_2_THRESHOLD                   0  /* always */
#define DIVEXACT_1_THRESHOLD                 0  /* always */
#define BMOD_1_TO_MOD_1_THRESHOLD           30

#define MUL_TOOM22_THRESHOLD                22
#define MUL_TOOM33_THRESHOLD                89
#define MUL_TOOM44_THRESHOLD               202
#define MUL_TOOM6H_THRESHOLD               270
#define MUL_TOOM8H_THRESHOLD               406

#define MUL_TOOM32_TO_TOOM43_THRESHOLD     129
#define MUL_TOOM32_TO_TOOM53_THRESHOLD     139
#define MUL_TOOM42_TO_TOOM53_THRESHOLD     127
#define MUL_TOOM42_TO_TOOM63_THRESHOLD     113

#define SQR_BASECASE_THRESHOLD               8
#define SQR_TOOM2_THRESHOLD                 52
#define SQR_TOOM3_THRESHOLD                125
#define SQR_TOOM4_THRESHOLD                226
#define SQR_TOOM6_THRESHOLD                306
#define SQR_TOOM8_THRESHOLD                430

#define MULMOD_BNM1_THRESHOLD               13
#define SQRMOD_BNM1_THRESHOLD               17

#define MUL_FFT_MODF_THRESHOLD             308  /* k = 5 */
#define MUL_FFT_TABLE3                                      \
  { {    308, 5}, {     17, 6}, {      9, 5}, {     19, 6}, \
    {     17, 7}, {      9, 6}, {     20, 7}, {     11, 6}, \
    {     23, 7}, {     13, 8}, {      7, 7}, {     15, 6}, \
    {     31, 7}, {     19, 8}, {     11, 7}, {     27, 9}, \
    {      7, 8}, {     15, 7}, {     33, 8}, {     19, 7}, \
    {     39, 8}, {     23, 7}, {     47, 8}, {     27, 9}, \
    {     15, 8}, {     39, 9}, {     23, 8}, {     47,10}, \
    {     15, 9}, {     31, 8}, {     63, 9}, {     39, 8}, \
    {     83, 9}, {     47,10}, {     31, 9}, {     79,10}, \
    {     47,11}, {   2048,12}, {   4096,13}, {   8192,14}, \
    {  16384,15}, {  32768,16} }
#define MUL_FFT_TABLE3_SIZE 42
#define MUL_FFT_THRESHOLD                 3520

#define SQR_FFT_MODF_THRESHOLD             276  /* k = 5 */
#define SQR_FFT_TABLE3                                      \
  { {    276, 5}, {     19, 6}, {     17, 7}, {      9, 6}, \
    {     20, 7}, {     11, 6}, {     23, 7}, {     19, 8}, \
    {     11, 7}, {     27, 8}, {     15, 7}, {     33, 8}, \
    {     19, 7}, {     39, 8}, {     23, 7}, {     47, 8}, \
    {     27, 9}, {     15, 8}, {     39, 9}, {     23, 8}, \
    {     47,10}, {     15, 9}, {     31, 8}, {     63, 9}, \
    {     39, 8}, {     79, 9}, {     47,10}, {     31, 9}, \
    {     79,10}, {     47,11}, {   2048,12}, {   4096,13}, \
    {   8192,14}, {  16384,15}, {  32768,16} }
#define SQR_FFT_TABLE3_SIZE 35
#define SQR_FFT_THRESHOLD                 2688

#define MULLO_BASECASE_THRESHOLD             0  /* always */
#define MULLO_DC_THRESHOLD                  54
#define MULLO_MUL_N_THRESHOLD             6633

#define DC_DIV_QR_THRESHOLD                 52
#define DC_DIVAPPR_Q_THRESHOLD             185
#define DC_BDIV_QR_THRESHOLD                53
#define DC_BDIV_Q_THRESHOLD                122

#define INV_MULMOD_BNM1_THRESHOLD           29
#define INV_NEWTON_THRESHOLD               260
#define INV_APPR_THRESHOLD                 220

#define BINV_NEWTON_THRESHOLD              230
#define REDC_1_TO_REDC_N_THRESHOLD          56

#define MU_DIV_QR_THRESHOLD               1142
#define MU_DIVAPPR_Q_THRESHOLD            1234
#define MUPI_DIV_QR_THRESHOLD              114
#define MU_BDIV_QR_THRESHOLD               792
#define MU_BDIV_Q_THRESHOLD               1099

#define MATRIX22_STRASSEN_THRESHOLD         15
#define HGCD_THRESHOLD                     151
#define GCD_DC_THRESHOLD                   599
#define GCDEXT_DC_THRESHOLD                460
#define JACOBI_BASE_METHOD                   2

#define GET_STR_DC_THRESHOLD                15
#define GET_STR_PRECOMPUTE_THRESHOLD        35
#define SET_STR_DC_THRESHOLD               915
#define SET_STR_PRECOMPUTE_THRESHOLD      1670
