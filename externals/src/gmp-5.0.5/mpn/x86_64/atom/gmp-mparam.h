/* Intel Atom/64 gmp-mparam.h -- Compiler/machine parameter header file.

Copyright 1991, 1993, 1994, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
2008, 2009, 2010 Free Software Foundation, Inc.

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
#define MOD_1N_TO_MOD_1_1_THRESHOLD         37
#define MOD_1U_TO_MOD_1_1_THRESHOLD          8
#define MOD_1_1_TO_MOD_1_2_THRESHOLD         0  /* never mpn_mod_1_1p */
#define MOD_1_2_TO_MOD_1_4_THRESHOLD        14
#define PREINV_MOD_1_TO_MOD_1_THRESHOLD     69
#define USE_PREINV_DIVREM_1                  1  /* native */
#define DIVEXACT_1_THRESHOLD                 0  /* always (native) */
#define BMOD_1_TO_MOD_1_THRESHOLD           32

#define MUL_TOOM22_THRESHOLD                10
#define MUL_TOOM33_THRESHOLD                66
#define MUL_TOOM44_THRESHOLD               118
#define MUL_TOOM6H_THRESHOLD               157
#define MUL_TOOM8H_THRESHOLD               236

#define MUL_TOOM32_TO_TOOM43_THRESHOLD      65
#define MUL_TOOM32_TO_TOOM53_THRESHOLD      76
#define MUL_TOOM42_TO_TOOM53_THRESHOLD      73
#define MUL_TOOM42_TO_TOOM63_THRESHOLD      66

#define SQR_BASECASE_THRESHOLD               0  /* always (native) */
#define SQR_TOOM2_THRESHOLD                 16
#define SQR_TOOM3_THRESHOLD                 65
#define SQR_TOOM4_THRESHOLD                166
#define SQR_TOOM6_THRESHOLD                226
#define SQR_TOOM8_THRESHOLD                333

#define MULMOD_BNM1_THRESHOLD                9
#define SQRMOD_BNM1_THRESHOLD                9

#define MUL_FFT_MODF_THRESHOLD             208  /* k = 5 */
#define MUL_FFT_TABLE3                                      \
  { {    208, 5}, {      7, 4}, {     15, 5}, {     11, 6}, \
    {      6, 5}, {     13, 6}, {      7, 5}, {     15, 6}, \
    {     13, 7}, {      7, 6}, {     15, 7}, {      9, 6}, \
    {     19, 7}, {     13, 8}, {      7, 7}, {     16, 8}, \
    {      9, 7}, {     19, 8}, {     11, 7}, {     23, 8}, \
    {     13, 9}, {      7, 8}, {     15, 7}, {     31, 8}, \
    {     19, 9}, {     11, 8}, {     25,10}, {      7, 9}, \
    {     15, 8}, {     33, 9}, {     19, 8}, {     39, 9}, \
    {     23,10}, {     15, 9}, {     39,10}, {     23, 9}, \
    {     47,11}, {     15,10}, {     31, 9}, {     63, 8}, \
    {    127, 9}, {     67,10}, {     39, 9}, {     79, 8}, \
    {    159,10}, {     47,11}, {     31,10}, {     63, 9}, \
    {    127, 8}, {    255, 7}, {    511,10}, {     71, 9}, \
    {    143, 8}, {    287, 7}, {    575,10}, {     79, 9}, \
    {    159, 8}, {    319,11}, {     47, 9}, {    191,12}, \
    {     31,11}, {     63,10}, {    127, 9}, {    255, 8}, \
    {    511,10}, {    143, 9}, {    287, 8}, {    575,11}, \
    {     79,10}, {    159, 9}, {    319,10}, {    175, 9}, \
    {    351, 8}, {    703, 7}, {   1407,10}, {    191, 9}, \
    {    415,11}, {    111,10}, {    223, 9}, {    447,12}, \
    {     63,11}, {    127,10}, {    255, 9}, {    511,11}, \
    {    143,10}, {    287, 9}, {    575, 8}, {   1151,10}, \
    {    319,11}, {    175,10}, {    351, 9}, {    703, 8}, \
    {   1407,11}, {    191,10}, {    383, 9}, {    767,10}, \
    {    415,11}, {    223,10}, {    447, 9}, {    895,13}, \
    {     63,12}, {    127,11}, {    255,10}, {    511,11}, \
    {    287,10}, {    575, 9}, {   1151,12}, {    159,11}, \
    {    319,10}, {    639,11}, {    351,10}, {    703, 9}, \
    {   1407,12}, {    191,11}, {    383,10}, {    767,11}, \
    {    415,12}, {    223,11}, {    447,10}, {    895,11}, \
    {    479,13}, {    127,12}, {    255,11}, {    511,12}, \
    {    287,11}, {    575,10}, {   1151,12}, {    319,11}, \
    {    639,12}, {    351,11}, {    703,10}, {   1407,13}, \
    {    191,12}, {    383,11}, {    767,12}, {    415,11}, \
    {    831,10}, {   1663,12}, {    447,11}, {    895,14}, \
    {    127,13}, {    255,12}, {    511,11}, {   1023,12}, \
    {    575,11}, {   1151,13}, {    319,12}, {    703,11}, \
    {   1407,13}, {    383,12}, {    831,13}, {    447,12}, \
    {    895,14}, {    255,13}, {    511,12}, {   1023,13}, \
    {    575,12}, {   1151,13}, {    703,12}, {   1407,14}, \
    {    383,13}, {    831,12}, {   1663,13}, {    895,15}, \
    {    255,14}, {    511,13}, {   1023,12}, {   2175,13}, \
    {   1151,14}, {    639,13}, {   1407,12}, {   2815,14}, \
    {    767,13}, {   1663,14}, {    895,13}, {   1919,15}, \
    {    511,14}, {   1023,13}, {   2047,14}, {   1151,13}, \
    {   2431,14}, {   1407,13}, {   2815,15}, {    767,14}, \
    {   1663,16}, {  65536,17}, { 131072,18}, { 262144,19}, \
    { 524288,20}, {1048576,21}, {2097152,22}, {4194304,23}, \
    {8388608,24} }
#define MUL_FFT_TABLE3_SIZE 193
#define MUL_FFT_THRESHOLD                 1728

#define SQR_FFT_MODF_THRESHOLD             208  /* k = 5 */
#define SQR_FFT_TABLE3                                      \
  { {    208, 5}, {      7, 4}, {     15, 5}, {     11, 6}, \
    {      6, 5}, {     13, 6}, {      7, 5}, {     15, 6}, \
    {     13, 7}, {      7, 6}, {     15, 7}, {      8, 6}, \
    {     17, 7}, {     17, 8}, {      9, 7}, {     19, 8}, \
    {     11, 7}, {     23, 8}, {     13, 9}, {      7, 8}, \
    {     19, 9}, {     11, 8}, {     25,10}, {      7, 9}, \
    {     15, 8}, {     31, 9}, {     19, 8}, {     39, 9}, \
    {     23,10}, {     15, 9}, {     39,10}, {     23, 9}, \
    {     47,11}, {     15,10}, {     31, 9}, {     67,10}, \
    {     39, 9}, {     79, 8}, {    159,10}, {     47,11}, \
    {     31,10}, {     63, 9}, {    127, 8}, {    255,10}, \
    {     71, 9}, {    143, 8}, {    287, 7}, {    575, 9}, \
    {    159, 8}, {    319,11}, {     47, 9}, {    191,12}, \
    {     31,11}, {     63,10}, {    127, 9}, {    255, 8}, \
    {    511,10}, {    143, 9}, {    287, 8}, {    575,10}, \
    {    159, 9}, {    319, 8}, {    639, 9}, {    351, 8}, \
    {    703,10}, {    191, 9}, {    383,10}, {    207, 9}, \
    {    415,11}, {    111,10}, {    223,12}, {     63,11}, \
    {    127,10}, {    255, 9}, {    511,11}, {    143,10}, \
    {    287, 9}, {    575,11}, {    159,10}, {    319, 9}, \
    {    639,11}, {    175,10}, {    351, 9}, {    703,11}, \
    {    191,10}, {    383,11}, {    207,10}, {    415,11}, \
    {    223,10}, {    447,13}, {     63,12}, {    127,11}, \
    {    255,10}, {    511,11}, {    287,10}, {    575,12}, \
    {    159,11}, {    319,10}, {    639,11}, {    351,10}, \
    {    703,12}, {    191,11}, {    383,10}, {    767,11}, \
    {    415,12}, {    223,11}, {    447,13}, {    127,12}, \
    {    255,11}, {    543,12}, {    287,11}, {    575,12}, \
    {    319,11}, {    639,12}, {    351,13}, {    191,12}, \
    {    383,11}, {    767,12}, {    415,11}, {    831,12}, \
    {    479,13}, {    255,10}, {   2047,12}, {    575,13}, \
    {    319,11}, {   1279,12}, {    703,13}, {    383,12}, \
    {    831,13}, {    447,12}, {    895,14}, {    255,13}, \
    {    511,12}, {   1023,13}, {    575,12}, {   1151,13}, \
    {    703,14}, {    383,13}, {    831,12}, {   1663,13}, \
    {    895,15}, {    255,14}, {    511,13}, {   1151,14}, \
    {    639,13}, {   1407,12}, {   2815,14}, {    767,13}, \
    {   1663,14}, {    895,13}, {   1791,15}, {  32768,16}, \
    {  65536,17}, { 131072,18}, { 262144,19}, { 524288,20}, \
    {1048576,21}, {2097152,22}, {4194304,23}, {8388608,24} }
#define SQR_FFT_TABLE3_SIZE 160
#define SQR_FFT_THRESHOLD                 1600

#define MULLO_BASECASE_THRESHOLD             0
#define MULLO_DC_THRESHOLD                  22
#define MULLO_MUL_N_THRESHOLD             3176

#define DC_DIV_QR_THRESHOLD                 26
#define DC_DIVAPPR_Q_THRESHOLD              93
#define DC_BDIV_QR_THRESHOLD                27
#define DC_BDIV_Q_THRESHOLD                 62

#define INV_MULMOD_BNM1_THRESHOLD           18
#define INV_NEWTON_THRESHOLD               131
#define INV_APPR_THRESHOLD                 110

#define BINV_NEWTON_THRESHOLD              165
#define REDC_1_TO_REDC_2_THRESHOLD          12
#define REDC_2_TO_REDC_N_THRESHOLD          36

#define MU_DIV_QR_THRESHOLD                792
#define MU_DIVAPPR_Q_THRESHOLD             807
#define MUPI_DIV_QR_THRESHOLD               67
#define MU_BDIV_QR_THRESHOLD               654
#define MU_BDIV_Q_THRESHOLD                792

#define MATRIX22_STRASSEN_THRESHOLD         13
#define HGCD_THRESHOLD                      83
#define GCD_DC_THRESHOLD                   198
#define GCDEXT_DC_THRESHOLD                198
#define JACOBI_BASE_METHOD                   2

#define GET_STR_DC_THRESHOLD                15
#define GET_STR_PRECOMPUTE_THRESHOLD        27
#define SET_STR_DC_THRESHOLD               254
#define SET_STR_PRECOMPUTE_THRESHOLD      1122
