/* Intel Pentium-4 gmp-mparam.h -- Compiler/machine parameter header file.

Copyright 1991, 1993, 1994, 2000, 2001, 2002, 2003, 2004, 2005, 2007, 2008,
2009, 2010 Free Software Foundation, Inc.

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


#define MOD_1_NORM_THRESHOLD                24
#define MOD_1_UNNORM_THRESHOLD           MP_SIZE_T_MAX  /* never */
#define MOD_1N_TO_MOD_1_1_THRESHOLD         26
#define MOD_1U_TO_MOD_1_1_THRESHOLD          9
#define MOD_1_1_TO_MOD_1_2_THRESHOLD         0  /* never mpn_mod_1_1p */
#define MOD_1_2_TO_MOD_1_4_THRESHOLD         0  /* never mpn_mod_1s_2p */
#define PREINV_MOD_1_TO_MOD_1_THRESHOLD     34
#define USE_PREINV_DIVREM_1                  1  /* native */
#define DIVEXACT_1_THRESHOLD                 0  /* always (native) */
#define BMOD_1_TO_MOD_1_THRESHOLD           22

#define MUL_TOOM22_THRESHOLD                30
#define MUL_TOOM33_THRESHOLD               120
#define MUL_TOOM44_THRESHOLD               296
#define MUL_TOOM6H_THRESHOLD               414
#define MUL_TOOM8H_THRESHOLD               620

#define MUL_TOOM32_TO_TOOM43_THRESHOLD     198
#define MUL_TOOM32_TO_TOOM53_THRESHOLD     216
#define MUL_TOOM42_TO_TOOM53_THRESHOLD     194
#define MUL_TOOM42_TO_TOOM63_THRESHOLD     209

#define SQR_BASECASE_THRESHOLD               0  /* always (native) */
#define SQR_TOOM2_THRESHOLD                 48
#define SQR_TOOM3_THRESHOLD                170
#define SQR_TOOM4_THRESHOLD                454
#define SQR_TOOM6_THRESHOLD                454
#define SQR_TOOM8_THRESHOLD                915

#define MULMOD_BNM1_THRESHOLD               19
#define SQRMOD_BNM1_THRESHOLD               24

#define MUL_FFT_MODF_THRESHOLD             904  /* k = 6 */
#define MUL_FFT_TABLE3                                      \
  { {    904, 6}, {     15, 5}, {     32, 6}, {     17, 5}, \
    {     35, 6}, {     19, 5}, {     39, 6}, {     28, 7}, \
    {     15, 6}, {     33, 7}, {     17, 6}, {     35, 7}, \
    {     19, 6}, {     41, 7}, {     21, 6}, {     43, 7}, \
    {     23, 6}, {     47, 7}, {     27, 6}, {     55, 8}, \
    {     15, 7}, {     31, 6}, {     63, 7}, {     35, 8}, \
    {     19, 7}, {     43, 8}, {     23, 7}, {     51, 8}, \
    {     27, 7}, {     55, 8}, {     31, 7}, {     63, 8}, \
    {     43, 9}, {     23, 8}, {     55, 9}, {     31, 8}, \
    {     71, 9}, {     39, 8}, {     79, 9}, {     47, 8}, \
    {     95, 9}, {     55,10}, {     31, 9}, {     63, 8}, \
    {    127, 9}, {     79,10}, {     47, 9}, {    103,11}, \
    {     31,10}, {     63, 9}, {    135,10}, {     79, 9}, \
    {    159,10}, {     95,11}, {     63,10}, {    127, 9}, \
    {    263,10}, {    143, 9}, {    287,10}, {    159,11}, \
    {     95,10}, {    207,12}, {     63,11}, {    127,10}, \
    {    271,11}, {    159,10}, {    319,11}, {    191,10}, \
    {    383,11}, {    223,12}, {    127,11}, {    287,10}, \
    {    607,11}, {    319,12}, {    191,11}, {    383,10}, \
    {    767,13}, {    127,12}, {    255,11}, {    511,10}, \
    {   1055,11}, {    543,10}, {   1119, 9}, {   2239,11}, \
    {    607,12}, {    319,11}, {    671,10}, {   1407,11}, \
    {    735,10}, {   1471, 9}, {   2943,12}, {    383,11}, \
    {    799,10}, {   1663,11}, {    863,10}, {   1727,12}, \
    {    447,13}, {    255,12}, {    511,11}, {   1055,10}, \
    {   2111,11}, {   1119,10}, {   2239, 9}, {   4479,12}, \
    {    575,11}, {   1247,10}, {   2495, 9}, {   4991,12}, \
    {    639,11}, {   1471,10}, {   2943,13}, {    383,12}, \
    {    767,11}, {   1599,12}, {    831,11}, {   1727,10}, \
    {   3455,14}, {    255,13}, {    511,12}, {   1023,11}, \
    {   2111,12}, {   1087,11}, {   2239,10}, {   4479,12}, \
    {   1215,11}, {   2495,10}, {   4991,13}, {    639,12}, \
    {   1471,11}, {   2943,10}, {   5887,11}, {   3007,13}, \
    {    767,12}, {   1727,11}, {   3455,13}, {    895,11}, \
    {   3839,12}, {   4096,13}, {   8192,14}, {  16384,15}, \
    {  32768,16} }
#define MUL_FFT_TABLE3_SIZE 141
#define MUL_FFT_THRESHOLD                 7552

#define SQR_FFT_MODF_THRESHOLD             793  /* k = 5 */
#define SQR_FFT_TABLE3                                      \
  { {    793, 5}, {     28, 6}, {     15, 5}, {     33, 6}, \
    {     17, 5}, {     35, 6}, {     19, 5}, {     39, 6}, \
    {     29, 7}, {     15, 6}, {     33, 7}, {     17, 6}, \
    {     35, 7}, {     19, 6}, {     41, 7}, {     23, 6}, \
    {     47, 7}, {     27, 6}, {     55, 7}, {     31, 6}, \
    {     63, 7}, {     37, 8}, {     19, 7}, {     43, 8}, \
    {     23, 7}, {     49, 8}, {     31, 7}, {     63, 8}, \
    {     39, 7}, {     79, 8}, {     43, 9}, {     23, 8}, \
    {     55, 9}, {     31, 8}, {     71, 9}, {     39, 8}, \
    {     79, 9}, {     47, 8}, {     95, 9}, {     55,10}, \
    {     31, 9}, {     79,10}, {     47, 9}, {    103,11}, \
    {     31,10}, {     63, 9}, {    135,10}, {     79, 9}, \
    {    159,10}, {     95, 9}, {    191,11}, {     63,10}, \
    {    159,11}, {     95,10}, {    191,12}, {     63,11}, \
    {    127,10}, {    255, 9}, {    511,10}, {    271,11}, \
    {    159,10}, {    335,11}, {    191,10}, {    383, 9}, \
    {    767,10}, {    399, 9}, {    799,11}, {    223,12}, \
    {    127,11}, {    255,10}, {    527, 9}, {   1055,10}, \
    {    543,11}, {    287,10}, {    607, 9}, {   1215,11}, \
    {    319,12}, {    191,11}, {    383,10}, {    799,13}, \
    {    127,12}, {    255,11}, {    511,10}, {   1055,11}, \
    {    543,10}, {   1119, 9}, {   2239,11}, {    607,10}, \
    {   1215,12}, {    319,11}, {    671,10}, {   1407,11}, \
    {    735,10}, {   1471, 9}, {   2943,10}, {   1503,12}, \
    {    383,11}, {    799,10}, {   1599,11}, {    863,10}, \
    {   1727,12}, {    447,11}, {    991,13}, {    255,12}, \
    {    511,11}, {   1055,10}, {   2111,11}, {   1119,10}, \
    {   2239,12}, {    575,11}, {   1247,10}, {   2495,12}, \
    {    639,11}, {   1471,10}, {   2943,13}, {    383,12}, \
    {    767,11}, {   1599,12}, {    831,11}, {   1727,10}, \
    {   3455,12}, {    959,11}, {   1919,14}, {    255,13}, \
    {    511,12}, {   1023,11}, {   2111,12}, {   1087,11}, \
    {   2239,10}, {   4479,12}, {   1215,11}, {   2495,13}, \
    {    639,12}, {   1471,11}, {   2943,10}, {   5887,13}, \
    {    767,12}, {   1727,11}, {   3455,13}, {    895,12}, \
    {   1791,11}, {   3711,12}, {   1919,11}, {   3839,12}, \
    {   4096,13}, {   8192,14}, {  16384,15}, {  32768,16} }
#define SQR_FFT_TABLE3_SIZE 148
#define SQR_FFT_THRESHOLD                 5760

#define MULLO_BASECASE_THRESHOLD            12
#define MULLO_DC_THRESHOLD                  51
#define MULLO_MUL_N_THRESHOLD            13463

#define DC_DIV_QR_THRESHOLD                 28
#define DC_DIVAPPR_Q_THRESHOLD              61
#define DC_BDIV_QR_THRESHOLD                55
#define DC_BDIV_Q_THRESHOLD                 82

#define INV_MULMOD_BNM1_THRESHOLD           60
#define INV_NEWTON_THRESHOLD                94
#define INV_APPR_THRESHOLD                  78

#define BINV_NEWTON_THRESHOLD              327
#define REDC_1_TO_REDC_N_THRESHOLD          63

#define MU_DIV_QR_THRESHOLD               2350
#define MU_DIVAPPR_Q_THRESHOLD            2089
#define MUPI_DIV_QR_THRESHOLD                7
#define MU_BDIV_QR_THRESHOLD              2089
#define MU_BDIV_Q_THRESHOLD               2089

#define MATRIX22_STRASSEN_THRESHOLD         34
#define HGCD_THRESHOLD                      74
#define GCD_DC_THRESHOLD                   321
#define GCDEXT_DC_THRESHOLD                209
#define JACOBI_BASE_METHOD                   1

#define GET_STR_DC_THRESHOLD                12
#define GET_STR_PRECOMPUTE_THRESHOLD        28
#define SET_STR_DC_THRESHOLD               123
#define SET_STR_PRECOMPUTE_THRESHOLD      1265
