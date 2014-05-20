/* POWER4/PowerPC970 gmp-mparam.h -- Compiler/machine parameter header file.

Copyright 2008, 2009, 2010 Free Software Foundation, Inc.

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


#define MOD_1_NORM_THRESHOLD                 0  /* always */
#define MOD_1_UNNORM_THRESHOLD               0  /* always */
#define MOD_1N_TO_MOD_1_1_THRESHOLD         10
#define MOD_1U_TO_MOD_1_1_THRESHOLD          6
#define MOD_1_1_TO_MOD_1_2_THRESHOLD         8
#define MOD_1_2_TO_MOD_1_4_THRESHOLD        23
#define PREINV_MOD_1_TO_MOD_1_THRESHOLD     16
#define USE_PREINV_DIVREM_1                  0
#define DIVEXACT_1_THRESHOLD                 0  /* always (native) */
#define BMOD_1_TO_MOD_1_THRESHOLD           43

#define MUL_TOOM22_THRESHOLD                14
#define MUL_TOOM33_THRESHOLD                54
#define MUL_TOOM44_THRESHOLD               154
#define MUL_TOOM6H_THRESHOLD               206
#define MUL_TOOM8H_THRESHOLD               309

#define MUL_TOOM32_TO_TOOM43_THRESHOLD      89
#define MUL_TOOM32_TO_TOOM53_THRESHOLD      99
#define MUL_TOOM42_TO_TOOM53_THRESHOLD      97
#define MUL_TOOM42_TO_TOOM63_THRESHOLD      97

#define SQR_BASECASE_THRESHOLD               5
#define SQR_TOOM2_THRESHOLD                 36
#define SQR_TOOM3_THRESHOLD                 61
#define SQR_TOOM4_THRESHOLD                154
#define SQR_TOOM6_THRESHOLD                206
#define SQR_TOOM8_THRESHOLD                309

#define MULMOD_BNM1_THRESHOLD               12
#define SQRMOD_BNM1_THRESHOLD               14

#define MUL_FFT_MODF_THRESHOLD             380  /* k = 5 */
#define MUL_FFT_TABLE3                                      \
  { {    380, 5}, {     17, 6}, {      9, 5}, {     19, 6}, \
    {     10, 5}, {     21, 6}, {     11, 5}, {     23, 6}, \
    {     23, 7}, {     12, 6}, {     25, 7}, {     25, 8}, \
    {     13, 7}, {     30, 6}, {     61, 7}, {     32, 8}, \
    {     17, 7}, {     35, 8}, {     29, 9}, {     15, 8}, \
    {     35, 9}, {     19, 8}, {     41, 9}, {     23, 8}, \
    {     47, 9}, {     27,10}, {     15, 9}, {     39,10}, \
    {     23, 9}, {     55,11}, {     15,10}, {     31, 9}, \
    {     71,10}, {     39, 9}, {     83,10}, {     47, 9}, \
    {     99,10}, {     55,11}, {     31,10}, {     63, 9}, \
    {    127,10}, {     79,11}, {     47,10}, {    103,12}, \
    {     31,11}, {     63,10}, {    127, 9}, {    255,10}, \
    {    135, 9}, {    271,11}, {     79,10}, {    159, 9}, \
    {    319,10}, {    167,11}, {     95,10}, {    191, 9}, \
    {    383, 8}, {    767,10}, {    207,11}, {    111,12}, \
    {     63,11}, {    127,10}, {    255, 9}, {    511,10}, \
    {    271,11}, {    143,10}, {    287, 9}, {    575,10}, \
    {    303, 9}, {    607,11}, {    159,10}, {    319, 9}, \
    {    639,10}, {    335, 9}, {    671,12}, {     95,11}, \
    {    191,10}, {    383, 9}, {    767,11}, {    207,10}, \
    {    415, 9}, {    831,13}, {     63,12}, {    127,11}, \
    {    255,10}, {    511,11}, {    271,10}, {    543, 9}, \
    {   1087,11}, {    287,10}, {    575,11}, {    303,10}, \
    {    607,12}, {    159,11}, {    319,10}, {    639,11}, \
    {    335,10}, {    671,11}, {    351,10}, {    703,11}, \
    {    367,12}, {    191,11}, {    383,10}, {    767,11}, \
    {    415,10}, {    831,12}, {    223,11}, {    447,10}, \
    {    895,13}, {    127,12}, {    255,11}, {    511,10}, \
    {   1023,11}, {    543,10}, {   1087,12}, {    287,11}, \
    {    575,10}, {   1151,11}, {    607,10}, {   1215,12}, \
    {    319,11}, {    639,10}, {   1279,11}, {    671,12}, \
    {    351,11}, {    703,10}, {   1407,13}, {    191,12}, \
    {    383,11}, {    767,12}, {    415,11}, {    831,10}, \
    {   1663,12}, {    447,11}, {    895,12}, {    479,14}, \
    {    127,13}, {    255,12}, {    511,11}, {   1023,12}, \
    {    543,11}, {   1087,10}, {   2175,12}, {    575,11}, \
    {   1151,12}, {    607,11}, {   1215,13}, {    319,12}, \
    {    639,11}, {   1279,12}, {    671,11}, {   1343,10}, \
    {   2687,12}, {    703,11}, {   1407,12}, {    735,13}, \
    {    383,12}, {    767,11}, {   1535,12}, {    799,11}, \
    {   1599,12}, {    831,11}, {   1663,13}, {    447,12}, \
    {    959,14}, {    255,13}, {    511,12}, {   1087,11}, \
    {   2175,13}, {    575,12}, {   1215,11}, {   2431,13}, \
    {    639,12}, {   1343,11}, {   2687,13}, {    703,12}, \
    {   1407,14}, {    383,13}, {    767,12}, {   1599,13}, \
    {    831,12}, {   1663,13}, {    959,15}, {    255,14}, \
    {    511,13}, {   1087,12}, {   2175,13}, {   1215,12}, \
    {   2431,14}, {    639,13}, {   1343,12}, {   2687,13}, \
    {   1471,12}, {   2943,14}, {    767,13}, {   1599,12}, \
    {   3199,13}, {   1663,14}, {    895,13}, {   1855,15}, \
    {  32768,16}, {  65536,17}, { 131072,18}, { 262144,19}, \
    { 524288,20}, {1048576,21}, {2097152,22}, {4194304,23}, \
    {8388608,24} }
#define MUL_FFT_TABLE3_SIZE 209
#define MUL_FFT_THRESHOLD                 7296

#define SQR_FFT_MODF_THRESHOLD             308  /* k = 5 */
#define SQR_FFT_TABLE3                                      \
  { {    308, 5}, {     17, 6}, {      9, 5}, {     19, 6}, \
    {     19, 7}, {     10, 6}, {     21, 7}, {     11, 6}, \
    {     23, 7}, {     21, 8}, {     11, 7}, {     24, 8}, \
    {     13, 7}, {     29, 8}, {     15, 7}, {     31, 8}, \
    {     21, 9}, {     11, 8}, {     27, 9}, {     15, 8}, \
    {     33, 9}, {     19, 8}, {     39, 9}, {     23, 8}, \
    {     47, 9}, {     27,10}, {     15, 9}, {     39,10}, \
    {     23, 9}, {     51,11}, {     15,10}, {     31, 9}, \
    {     67,10}, {     39, 9}, {     83,10}, {     47, 9}, \
    {     95,10}, {     55,11}, {     31,10}, {     79,11}, \
    {     47,10}, {     95, 9}, {    191, 8}, {    383,12}, \
    {     31,11}, {     63,10}, {    127, 9}, {    255, 8}, \
    {    511,10}, {    135, 9}, {    271,11}, {     79,10}, \
    {    159, 9}, {    319,10}, {    175, 9}, {    351,11}, \
    {     95,10}, {    191, 9}, {    383,10}, {    207, 9}, \
    {    415,11}, {    111,12}, {     63,11}, {    127,10}, \
    {    255, 9}, {    511, 8}, {   1023,10}, {    271, 9}, \
    {    543,10}, {    287, 9}, {    575, 8}, {   1151,10}, \
    {    303,11}, {    159,10}, {    319, 9}, {    639,11}, \
    {    175,10}, {    351,12}, {     95,11}, {    191,10}, \
    {    383, 9}, {    767,11}, {    207,10}, {    415, 9}, \
    {    831,11}, {    223,13}, {     63,12}, {    127,11}, \
    {    255,10}, {    511, 9}, {   1023,11}, {    271,10}, \
    {    543,11}, {    287,10}, {    575, 9}, {   1151,11}, \
    {    303,12}, {    159,11}, {    319,10}, {    639,11}, \
    {    351,10}, {    703,12}, {    191,11}, {    383,10}, \
    {    767,11}, {    415,10}, {    831,12}, {    223,11}, \
    {    447,10}, {    895,11}, {    479,10}, {    959,13}, \
    {    127,12}, {    255,11}, {    511,10}, {   1023,11}, \
    {    543,12}, {    287,11}, {    575,10}, {   1151,11}, \
    {    607,12}, {    319,11}, {    639,10}, {   1279,12}, \
    {    351,11}, {    703,13}, {    191,12}, {    383,11}, \
    {    767,12}, {    415,11}, {    831,10}, {   1663,12}, \
    {    447,11}, {    895,12}, {    479,11}, {    959,14}, \
    {    127,13}, {    255,12}, {    511,11}, {   1023,12}, \
    {    543,11}, {   1087,10}, {   2175,12}, {    575,11}, \
    {   1151,12}, {    607,13}, {    319,12}, {    639,11}, \
    {   1279,12}, {    671,11}, {   1343,12}, {    703,11}, \
    {   1407,13}, {    383,12}, {    767,11}, {   1535,12}, \
    {    831,11}, {   1663,13}, {    447,12}, {    959,11}, \
    {   1919,14}, {    255,13}, {    511,12}, {   1087,11}, \
    {   2175,13}, {    575,12}, {   1215,11}, {   2431,13}, \
    {    639,12}, {   1343,13}, {    703,12}, {   1407,14}, \
    {    383,13}, {    767,12}, {   1535,13}, {    831,12}, \
    {   1663,13}, {    959,12}, {   1919,15}, {    255,14}, \
    {    511,13}, {   1087,12}, {   2175,13}, {   1215,12}, \
    {   2431,14}, {    639,13}, {   1343,12}, {   2687,13}, \
    {   1407,12}, {   2815,13}, {   1471,14}, {    767,13}, \
    {   1535,12}, {   3071,13}, {   1663,14}, {    895,13}, \
    {   1791,12}, {   3839,15}, {  32768,16}, {  65536,17}, \
    { 131072,18}, { 262144,19}, { 524288,20}, {1048576,21}, \
    {2097152,22}, {4194304,23}, {8388608,24} }
#define SQR_FFT_TABLE3_SIZE 207
#define SQR_FFT_THRESHOLD                 2752

#define MULLO_BASECASE_THRESHOLD             5
#define MULLO_DC_THRESHOLD                  34
#define MULLO_MUL_N_THRESHOLD            10950

#define DC_DIV_QR_THRESHOLD                 30
#define DC_DIVAPPR_Q_THRESHOLD             103
#define DC_BDIV_QR_THRESHOLD                48
#define DC_BDIV_Q_THRESHOLD                120

#define INV_MULMOD_BNM1_THRESHOLD           50
#define INV_NEWTON_THRESHOLD               131
#define INV_APPR_THRESHOLD                 115

#define BINV_NEWTON_THRESHOLD              204
#define REDC_1_TO_REDC_N_THRESHOLD          55

#define MU_DIV_QR_THRESHOLD                998
#define MU_DIVAPPR_Q_THRESHOLD             998
#define MUPI_DIV_QR_THRESHOLD               61
#define MU_BDIV_QR_THRESHOLD               889
#define MU_BDIV_Q_THRESHOLD               1078

#define MATRIX22_STRASSEN_THRESHOLD         11
#define HGCD_THRESHOLD                      96
#define GCD_DC_THRESHOLD                   249
#define GCDEXT_DC_THRESHOLD                209
#define JACOBI_BASE_METHOD                   1

#define GET_STR_DC_THRESHOLD                11
#define GET_STR_PRECOMPUTE_THRESHOLD        23
#define SET_STR_DC_THRESHOLD               532
#define SET_STR_PRECOMPUTE_THRESHOLD      1781
