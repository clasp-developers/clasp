/* Sandy Bridge gmp-mparam.h -- Compiler/machine parameter header file.

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

#define GMP_LIMB_BITS 64
#define BYTES_PER_MP_LIMB 8

/* 3300 MHz Core i5 Sandy Bridge */

#define MOD_1_NORM_THRESHOLD                 0  /* always */
#define MOD_1_UNNORM_THRESHOLD               0  /* always */
#define MOD_1N_TO_MOD_1_1_THRESHOLD          6
#define MOD_1U_TO_MOD_1_1_THRESHOLD          6
#define MOD_1_1_TO_MOD_1_2_THRESHOLD         0  /* never mpn_mod_1_1p */
#define MOD_1_2_TO_MOD_1_4_THRESHOLD        22
#define PREINV_MOD_1_TO_MOD_1_THRESHOLD     15
#define USE_PREINV_DIVREM_1                  1  /* native */
#define DIVEXACT_1_THRESHOLD                 0  /* always (native) */
#define BMOD_1_TO_MOD_1_THRESHOLD           34

#define MUL_TOOM22_THRESHOLD                20
#define MUL_TOOM33_THRESHOLD                57
#define MUL_TOOM44_THRESHOLD               166
#define MUL_TOOM6H_THRESHOLD               387
#define MUL_TOOM8H_THRESHOLD               527

#define MUL_TOOM32_TO_TOOM43_THRESHOLD     105
#define MUL_TOOM32_TO_TOOM53_THRESHOLD     114
#define MUL_TOOM42_TO_TOOM53_THRESHOLD     113
#define MUL_TOOM42_TO_TOOM63_THRESHOLD     114

#define SQR_BASECASE_THRESHOLD               0  /* always (native) */
#define SQR_TOOM2_THRESHOLD                 30
#define SQR_TOOM3_THRESHOLD                 93
#define SQR_TOOM4_THRESHOLD                278
#define SQR_TOOM6_THRESHOLD                369
#define SQR_TOOM8_THRESHOLD                557

#define MULMOD_BNM1_THRESHOLD               13
#define SQRMOD_BNM1_THRESHOLD               18

#define MUL_FFT_MODF_THRESHOLD             376  /* k = 5 */
#define MUL_FFT_TABLE3                                      \
  { {    376, 5}, {     17, 6}, {      9, 5}, {     21, 6}, \
    {     11, 5}, {     23, 6}, {     21, 7}, {     11, 6}, \
    {     23, 7}, {     12, 6}, {     25, 7}, {     13, 6}, \
    {     27, 7}, {     21, 8}, {     11, 7}, {     25, 8}, \
    {     13, 7}, {     27, 8}, {     15, 7}, {     31, 8}, \
    {     17, 7}, {     35, 8}, {     19, 7}, {     39, 8}, \
    {     21, 9}, {     11, 8}, {     27, 9}, {     15, 8}, \
    {     35, 9}, {     19, 8}, {     41, 9}, {     23, 8}, \
    {     47, 9}, {     27,10}, {     15, 9}, {     39,10}, \
    {     23, 9}, {     51,11}, {     15,10}, {     31, 9}, \
    {     67,10}, {     39, 9}, {     79,10}, {     47, 9}, \
    {     95,10}, {     55,11}, {     31,10}, {     79,11}, \
    {     47,10}, {     95,12}, {     31,11}, {     63,10}, \
    {    135,11}, {     79,10}, {    159,11}, {     95,10}, \
    {    191, 8}, {    767,12}, {     63,11}, {    127,10}, \
    {    255, 9}, {    511,11}, {    143,10}, {    287,11}, \
    {    159, 9}, {    639,12}, {     95,11}, {    191,13}, \
    {     63,12}, {    127,10}, {    511,11}, {    271,10}, \
    {    543, 9}, {   1087,10}, {    607,12}, {    159,11}, \
    {    319,10}, {    639,11}, {    335,10}, {    671,11}, \
    {    351,10}, {    703, 9}, {   1407,10}, {    735,12}, \
    {    191,11}, {    415,10}, {    831,12}, {    223,11}, \
    {    447,13}, {    127,12}, {    255,11}, {    543,12}, \
    {    287,11}, {    607,12}, {    319,11}, {    639,12}, \
    {    351,11}, {    703,13}, {    191,12}, {    383,11}, \
    {    767,12}, {    415,11}, {    831,12}, {    447,11}, \
    {    895,12}, {    479,14}, {    127,13}, {    255,12}, \
    {    543,11}, {   1087,12}, {    607,13}, {    319,12}, \
    {    735,13}, {    383,12}, {    831,11}, {   1663,13}, \
    {    447,12}, {    959,11}, {   1919,13}, {    511,12}, \
    {   1087,11}, {   2175,13}, {    575,12}, {   1215,11}, \
    {   2431,13}, {    639,12}, {   1279,13}, {    703,12}, \
    {   1407,14}, {    383,13}, {    767,12}, {   1535,13}, \
    {    831,12}, {   1727,13}, {    959,12}, {   1919,14}, \
    {    511,13}, {   1087,12}, {   2175,13}, {   1215,12}, \
    {   2431,14}, {    639,13}, {   1343,12}, {   2687,13}, \
    {   1471,12}, {   2943,14}, {    767,13}, {   1663,14}, \
    {    895,13}, {   1919,15}, {    511,14}, {   1023,13}, \
    {   2175,14}, {   1151,13}, {   2431,12}, {   4863,14}, \
    {   1279,13}, {   2687,14}, {   1407,13}, {   2943,15}, \
    {    767,14}, {   1535,13}, {   3199,14}, {   1663,13}, \
    {   3455,14}, {   1919,16}, {    511,15}, {   1023,14}, \
    {   2431,13}, {   4863,15}, {   1279,14}, {   2943,13}, \
    {   5887,15}, {   1535,14}, {  16384,15}, {  32768,16}, \
    {  65536,17}, { 131072,18}, { 262144,19}, { 524288,20}, \
    {1048576,21}, {2097152,22}, {4194304,23}, {8388608,24} }
#define MUL_FFT_TABLE3_SIZE 184
#define MUL_FFT_THRESHOLD                 3712

#define SQR_FFT_MODF_THRESHOLD             336  /* k = 5 */
#define SQR_FFT_TABLE3                                      \
  { {    336, 5}, {     19, 6}, {     10, 5}, {     21, 6}, \
    {     21, 7}, {     11, 6}, {     23, 7}, {     12, 6}, \
    {     25, 7}, {     25, 8}, {     13, 7}, {     27, 8}, \
    {     15, 7}, {     31, 8}, {     17, 7}, {     35, 8}, \
    {     21, 9}, {     11, 8}, {     27, 9}, {     15, 8}, \
    {     35, 9}, {     19, 8}, {     41, 9}, {     23, 8}, \
    {     47, 9}, {     27,10}, {     15, 9}, {     39,10}, \
    {     23, 9}, {     51,11}, {     15,10}, {     31, 9}, \
    {     63,10}, {     39, 9}, {     79,10}, {     47,11}, \
    {     31,10}, {     79,11}, {     47,10}, {     95,12}, \
    {     31,11}, {     63,10}, {    127, 9}, {    255,10}, \
    {    135,11}, {     79, 8}, {    639,11}, {     95,10}, \
    {    191, 9}, {    383,12}, {     63, 9}, {    511,10}, \
    {    271,11}, {    143,10}, {    287, 9}, {    575,11}, \
    {    159,10}, {    319,12}, {     95,11}, {    191,10}, \
    {    383,11}, {    207,10}, {    415,13}, {     63,12}, \
    {    127,11}, {    255,10}, {    575,11}, {    303,10}, \
    {    639,11}, {    351,10}, {    703,12}, {    191,11}, \
    {    383,10}, {    767,11}, {    415,10}, {    831,12}, \
    {    223,11}, {    447,10}, {    959,13}, {    127,11}, \
    {    511,10}, {   1023,11}, {    607,10}, {   1215,12}, \
    {    319,11}, {    671,12}, {    351,11}, {    703,13}, \
    {    191,12}, {    383,11}, {    767,12}, {    415,11}, \
    {    831,12}, {    447,11}, {    895,12}, {    479,11}, \
    {    959,14}, {    127,13}, {    255,12}, {    543,11}, \
    {   1087,12}, {    575,11}, {   1151,12}, {    607,13}, \
    {    319,12}, {    671,11}, {   1343,12}, {    703,13}, \
    {    383,12}, {    831,13}, {    447,12}, {    959,11}, \
    {   1919,13}, {    511,12}, {   1023,13}, {    575,12}, \
    {   1215,13}, {    639,12}, {   1343,13}, {    703,14}, \
    {    383,13}, {    767,12}, {   1535,13}, {    831,12}, \
    {   1663,13}, {    959,12}, {   1919,14}, {    511,13}, \
    {   1087,12}, {   2175,13}, {   1215,14}, {    639,13}, \
    {   1343,12}, {   2687,13}, {   1407,12}, {   2815,13}, \
    {   1471,14}, {    767,13}, {   1599,12}, {   3199,13}, \
    {   1663,14}, {    895,13}, {   1919,15}, {    511,14}, \
    {   1023,13}, {   2175,14}, {   1151,13}, {   2431,12}, \
    {   4863,14}, {   1279,13}, {   2687,14}, {   1407,13}, \
    {   2815,15}, {    767,14}, {   1535,13}, {   3199,14}, \
    {   1663,13}, {   3455,14}, {   1919,16}, {    511,15}, \
    {   1023,14}, {   2431,13}, {   4863,15}, {   1279,14}, \
    {   2943,13}, {   5887,15}, {   1535,14}, {  16384,15}, \
    {  32768,16}, {  65536,17}, { 131072,18}, { 262144,19}, \
    { 524288,20}, {1048576,21}, {2097152,22}, {4194304,23}, \
    {8388608,24} }
#define SQR_FFT_TABLE3_SIZE 177
#define SQR_FFT_THRESHOLD                 3264

#define MULLO_BASECASE_THRESHOLD             5
#define MULLO_DC_THRESHOLD                  33
#define MULLO_MUL_N_THRESHOLD             6633

#define DC_DIV_QR_THRESHOLD                 39
#define DC_DIVAPPR_Q_THRESHOLD             119
#define DC_BDIV_QR_THRESHOLD                31
#define DC_BDIV_Q_THRESHOLD                 78

#define INV_MULMOD_BNM1_THRESHOLD           46
#define INV_NEWTON_THRESHOLD               139
#define INV_APPR_THRESHOLD                 131

#define BINV_NEWTON_THRESHOLD              198
#define REDC_1_TO_REDC_2_THRESHOLD          23
#define REDC_2_TO_REDC_N_THRESHOLD          59

#define MU_DIV_QR_THRESHOLD               1334
#define MU_DIVAPPR_Q_THRESHOLD            1442
#define MUPI_DIV_QR_THRESHOLD               66
#define MU_BDIV_QR_THRESHOLD              1017
#define MU_BDIV_Q_THRESHOLD               1442

#define MATRIX22_STRASSEN_THRESHOLD         15
#define HGCD_THRESHOLD                     125 /* hardwired, tuneup crashes */
#define GCD_DC_THRESHOLD                   396
#define GCDEXT_DC_THRESHOLD                368
#define JACOBI_BASE_METHOD                   1

#define GET_STR_DC_THRESHOLD                12
#define GET_STR_PRECOMPUTE_THRESHOLD        21
#define SET_STR_DC_THRESHOLD               650
#define SET_STR_PRECOMPUTE_THRESHOLD      1585
