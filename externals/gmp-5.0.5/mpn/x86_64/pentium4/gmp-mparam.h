/* Pentium 4-64 gmp-mparam.h -- Compiler/machine parameter header file.

Copyright 1991, 1993, 1994, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
2008, 2009, 2012 Free Software Foundation, Inc.

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

#define GMP_LIMB_BITS 64
#define BYTES_PER_MP_LIMB 8

/* These routines exists for all x86_64 chips, but they are slower on Pentium4
   than separate add/sub and shift.  Make sure they are not really used.  */
#undef HAVE_NATIVE_mpn_rsh1add_n
#undef HAVE_NATIVE_mpn_rsh1sub_n

/* 3200 MHz Pentium / 2048 Kibyte cache / socket 775 */

#define MOD_1_NORM_THRESHOLD                 0  /* always */
#define MOD_1_UNNORM_THRESHOLD               0  /* always */
#define MOD_1N_TO_MOD_1_1_THRESHOLD          6
#define MOD_1U_TO_MOD_1_1_THRESHOLD          4
#define MOD_1_1_TO_MOD_1_2_THRESHOLD         9
#define MOD_1_2_TO_MOD_1_4_THRESHOLD        16
#define PREINV_MOD_1_TO_MOD_1_THRESHOLD     11
#define USE_PREINV_DIVREM_1                  1  /* native */
#define DIVEXACT_1_THRESHOLD                 0  /* always (native) */
#define BMOD_1_TO_MOD_1_THRESHOLD           24

#define MUL_TOOM22_THRESHOLD                12
#define MUL_TOOM33_THRESHOLD                81
#define MUL_TOOM44_THRESHOLD               121
#define MUL_TOOM6H_THRESHOLD               270
#define MUL_TOOM8H_THRESHOLD               430

#define MUL_TOOM32_TO_TOOM43_THRESHOLD      81
#define MUL_TOOM32_TO_TOOM53_THRESHOLD     138
#define MUL_TOOM42_TO_TOOM53_THRESHOLD     144
#define MUL_TOOM42_TO_TOOM63_THRESHOLD      88

#define SQR_BASECASE_THRESHOLD               0  /* always (native) */
#define SQR_TOOM2_THRESHOLD                 20
#define SQR_TOOM3_THRESHOLD                 81
#define SQR_TOOM4_THRESHOLD                226
#define SQR_TOOM6_THRESHOLD                303
#define SQR_TOOM8_THRESHOLD                454

#define MULMOD_BNM1_THRESHOLD                9
#define SQRMOD_BNM1_THRESHOLD               11


#define MUL_FFT_MODF_THRESHOLD             240  /* k = 5 */
#define MUL_FFT_TABLE3                                      \
  { {    240, 5}, {      9, 4}, {     19, 5}, {     11, 6}, \
    {      6, 5}, {     13, 6}, {      7, 5}, {     15, 6}, \
    {      8, 5}, {     17, 6}, {      9, 5}, {     19, 6}, \
    {     13, 7}, {      7, 6}, {     15, 7}, {      8, 6}, \
    {     17, 7}, {      9, 6}, {     19, 7}, {     11, 6}, \
    {     23, 7}, {     13, 8}, {      7, 7}, {     17, 8}, \
    {      9, 7}, {     21, 8}, {     11, 7}, {     23, 8}, \
    {     13, 9}, {      7, 8}, {     15, 7}, {     31, 8}, \
    {     21, 9}, {     11, 8}, {     25,10}, {      7, 9}, \
    {     15, 8}, {     33, 9}, {     19, 8}, {     39, 9}, \
    {     23, 8}, {     47, 9}, {     27,10}, {     15, 9}, \
    {     39,10}, {     23, 9}, {     51,11}, {     15,10}, \
    {     31, 9}, {     63,10}, {     39, 9}, {     79,10}, \
    {     47,11}, {     31,10}, {     63, 9}, {    127, 8}, \
    {    255,10}, {     79, 9}, {    159,11}, {     47,10}, \
    {     95,12}, {     31,11}, {     63,10}, {    127, 9}, \
    {    287,11}, {     79,10}, {    159, 9}, {    319,10}, \
    {    175,11}, {     95,10}, {    191, 9}, {    383,10}, \
    {    207, 9}, {    415,11}, {    111,10}, {    223,12}, \
    {     63,11}, {    127,10}, {    255,11}, {    143,10}, \
    {    287,11}, {    159,10}, {    319,11}, {    175,12}, \
    {     95,11}, {    223,13}, {     63,12}, {    127,11}, \
    {    287,10}, {    575,12}, {    159,11}, {    319,10}, \
    {    639,11}, {    351,12}, {    191,11}, {    383,12}, \
    {    223,11}, {    447,13}, {    127,12}, {    255,11}, \
    {    511,12}, {    287,11}, {    575,12}, {    319,11}, \
    {    639,12}, {    351,13}, {    191,12}, {    415,11}, \
    {    831,12}, {    447,14}, {    127,13}, {    255,12}, \
    {    511,11}, {   1023,12}, {    543,11}, {   1087,10}, \
    {   2175,12}, {    575,13}, {    319,12}, {    639,11}, \
    {   1279,12}, {    703,11}, {   1407,13}, {    383,12}, \
    {    767,11}, {   1535,12}, {    831,11}, {   1663,13}, \
    {    447,14}, {    255,13}, {    511,12}, {   1023,11}, \
    {   2047,12}, {   1087,11}, {   2175,13}, {    575,12}, \
    {   1151,11}, {   2303,12}, {   1215,11}, {   2431,10}, \
    {   4863,13}, {    639,12}, {   1279,11}, {   2559,13}, \
    {    703,12}, {   1407,14}, {    383,13}, {    767,12}, \
    {   1535,13}, {    831,12}, {   1663,13}, {    895,15}, \
    {    255,14}, {    511,13}, {   1023,12}, {   2047,13}, \
    {   1087,12}, {   2175,13}, {   1215,12}, {   2431,11}, \
    {   4863,14}, {    639,13}, {   1407,12}, {   2815,13}, \
    {   1471,14}, {    767,13}, {   1663,14}, {    895,13}, \
    {   1791,12}, {   3583,13}, {   1919,12}, {   3839,15}, \
    {    511,14}, {   1023,13}, {   2175,14}, {   1151,13}, \
    {   2303,12}, {   4607,13}, {   2431,12}, {   4863,14}, \
    {   1279,13}, {   2687,14}, {   1407,13}, {   2815,15}, \
    {    767,14}, {   1791,13}, {   3583,14}, {   1919,13}, \
    {   3839,12}, {   7679,16}, {    511,15}, {   1023,14}, \
    {   2303,13}, {   4607,14}, {   2431,13}, {   4863,15}, \
    {   1279,14}, {   2943,13}, {   5887,15}, {   1535,14}, \
    {   3199,15}, {   1791,14}, {   3839,13}, {   7679,16}, \
    {   1023,15}, {   2047,14}, {   4351,15}, {   2303,14}, \
    {   4863,15}, {   2815,14}, {   5887,13}, {  11775,16}, \
    {   1535,15}, {   3071,14}, {   6655,15}, {  32768,16}, \
    {  65536,17}, { 131072,18}, { 262144,19}, { 524288,20}, \
    {1048576,21}, {2097152,22}, {4194304,23}, {8388608,24} }
#define MUL_FFT_TABLE3_SIZE 224
#define MUL_FFT_THRESHOLD                 2752

#define SQR_FFT_MODF_THRESHOLD             240  /* k = 5 */
#define SQR_FFT_TABLE3                                      \
  { {    240, 5}, {     11, 6}, {      6, 5}, {     13, 6}, \
    {     15, 7}, {      8, 6}, {     19, 7}, {     10, 6}, \
    {     21, 7}, {     13, 8}, {      7, 7}, {     21, 8}, \
    {     11, 7}, {     25, 8}, {     13, 9}, {      7, 8}, \
    {     15, 7}, {     31, 8}, {     21, 9}, {     11, 8}, \
    {     25,10}, {      7, 9}, {     15, 8}, {     33, 9}, \
    {     19, 8}, {     39, 9}, {     23, 8}, {     47, 9}, \
    {     27,10}, {     15, 9}, {     39,10}, {     23, 9}, \
    {     51,11}, {     15,10}, {     31, 9}, {     63, 8}, \
    {    127,10}, {     39, 9}, {     79,10}, {     47,11}, \
    {     31,10}, {     63, 9}, {    127, 8}, {    255,10}, \
    {     71, 9}, {    143, 7}, {    575,10}, {     79,11}, \
    {     47,12}, {     31,11}, {     63,10}, {    127, 9}, \
    {    255,10}, {    143, 9}, {    287,11}, {     79, 9}, \
    {    319,10}, {    191, 9}, {    383,10}, {    207,12}, \
    {     63,11}, {    127,10}, {    255,11}, {    143,10}, \
    {    287, 9}, {    575,11}, {    159,10}, {    319,11}, \
    {    175,10}, {    351,12}, {     95,11}, {    191,10}, \
    {    383,11}, {    223,13}, {     63,12}, {    127,11}, \
    {    287,12}, {    159,11}, {    351,12}, {    191,11}, \
    {    383,10}, {    767,11}, {    415,12}, {    223,11}, \
    {    447,13}, {    127,12}, {    255,11}, {    511,12}, \
    {    287,11}, {    575,12}, {    319,11}, {    639,12}, \
    {    351,13}, {    191,12}, {    383,11}, {    767,12}, \
    {    415,11}, {    831,12}, {    447,14}, {    127,13}, \
    {    255,12}, {    511,11}, {   1023,12}, {    543,11}, \
    {   1087,10}, {   2175,12}, {    575,13}, {    319,12}, \
    {    639,11}, {   1279,12}, {    671,11}, {   1343,12}, \
    {    703,13}, {    383,12}, {    767,11}, {   1535,12}, \
    {    831,13}, {    447,14}, {    255,13}, {    511,12}, \
    {   1023,11}, {   2047,12}, {   1087,11}, {   2175,13}, \
    {    575,12}, {   1151,11}, {   2303,12}, {   1215,13}, \
    {    639,12}, {   1279,11}, {   2559,12}, {   1343,13}, \
    {    703,14}, {    383,13}, {    767,12}, {   1535,13}, \
    {    831,12}, {   1663,15}, {    255,14}, {    511,13}, \
    {   1023,12}, {   2047,13}, {   1087,12}, {   2175,13}, \
    {   1151,12}, {   2303,13}, {   1215,14}, {    639,13}, \
    {   1343,12}, {   2687,13}, {   1407,12}, {   2815,14}, \
    {    767,13}, {   1663,14}, {    895,13}, {   1791,12}, \
    {   3583,13}, {   1919,12}, {   3839,15}, {    511,14}, \
    {   1023,13}, {   2175,14}, {   1151,13}, {   2303,12}, \
    {   4607,13}, {   2431,12}, {   4863,14}, {   1279,13}, \
    {   2687,14}, {   1407,13}, {   2815,15}, {    767,14}, \
    {   1535,13}, {   3071,14}, {   1791,13}, {   3583,14}, \
    {   1919,13}, {   3839,12}, {   7679,16}, {    511,15}, \
    {   1023,14}, {   2175,13}, {   4351,14}, {   2303,13}, \
    {   4607,14}, {   2431,13}, {   4863,15}, {   1279,14}, \
    {   2815,13}, {   5631,14}, {   2943,13}, {   5887,12}, \
    {  11775,15}, {   1535,14}, {   3199,15}, {   1791,14}, \
    {   3583,13}, {   7167,14}, {   3839,13}, {   7679,16}, \
    {   1023,15}, {   2047,14}, {   4351,15}, {   2303,14}, \
    {   4863,15}, {   2815,14}, {   5887,13}, {  11775,16}, \
    {   1535,15}, {   3071,14}, {   6655,15}, {   3583,14}, \
    {   7167,15}, {  32768,16}, {  65536,17}, { 131072,18}, \
    { 262144,19}, { 524288,20}, {1048576,21}, {2097152,22}, \
    {4194304,23}, {8388608,24} }
#define SQR_FFT_TABLE3_SIZE 222
#define SQR_FFT_THRESHOLD                 2240

#define MULLO_BASECASE_THRESHOLD             0  /* always */
#define MULLO_DC_THRESHOLD                  27
#define MULLO_MUL_N_THRESHOLD             5240

#define DC_DIV_QR_THRESHOLD                 28
#define DC_DIVAPPR_Q_THRESHOLD              60
#define DC_BDIV_QR_THRESHOLD                31
#define DC_BDIV_Q_THRESHOLD                 49

#define INV_MULMOD_BNM1_THRESHOLD           22
#define INV_NEWTON_THRESHOLD               226
#define INV_APPR_THRESHOLD                 108

#define BINV_NEWTON_THRESHOLD              262
#define REDC_1_TO_REDC_2_THRESHOLD          11
#define REDC_2_TO_REDC_N_THRESHOLD          44

#define MU_DIV_QR_THRESHOLD                979
#define MU_DIVAPPR_Q_THRESHOLD            1078
#define MUPI_DIV_QR_THRESHOLD               91
#define MU_BDIV_QR_THRESHOLD               792
#define MU_BDIV_Q_THRESHOLD                942

#define MATRIX22_STRASSEN_THRESHOLD         21
#define HGCD_THRESHOLD                      97
#define GCD_DC_THRESHOLD                   217
#define GCDEXT_DC_THRESHOLD                237
#define JACOBI_BASE_METHOD                   1

#define GET_STR_DC_THRESHOLD                12
#define GET_STR_PRECOMPUTE_THRESHOLD        23
#define SET_STR_DC_THRESHOLD               572
#define SET_STR_PRECOMPUTE_THRESHOLD      1588
