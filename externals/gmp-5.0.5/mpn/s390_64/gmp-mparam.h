/* S/390-64 gmp-mparam.h -- Compiler/machine parameter header file.

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

#define DIVREM_1_NORM_THRESHOLD              0  /* always */
#define DIVREM_1_UNNORM_THRESHOLD            0  /* always */
#define MOD_1_NORM_THRESHOLD                 0  /* always */
#define MOD_1_UNNORM_THRESHOLD               0  /* always */
#define MOD_1N_TO_MOD_1_1_THRESHOLD      MP_SIZE_T_MAX  /* never */
#define MOD_1U_TO_MOD_1_1_THRESHOLD          9
#define MOD_1_1_TO_MOD_1_2_THRESHOLD         0  /* never mpn_mod_1_1p */
#define MOD_1_2_TO_MOD_1_4_THRESHOLD        19
#define PREINV_MOD_1_TO_MOD_1_THRESHOLD  MP_SIZE_T_MAX  /* never */
#define USE_PREINV_DIVREM_1                  1
#define DIVREM_2_THRESHOLD                   0  /* always */
#define DIVEXACT_1_THRESHOLD                 0  /* always */
#define BMOD_1_TO_MOD_1_THRESHOLD          101

#define MUL_TOOM22_THRESHOLD                14
#define MUL_TOOM33_THRESHOLD                74
#define MUL_TOOM44_THRESHOLD               118
#define MUL_TOOM6H_THRESHOLD               157
#define MUL_TOOM8H_THRESHOLD               236

#define MUL_TOOM32_TO_TOOM43_THRESHOLD      73
#define MUL_TOOM32_TO_TOOM53_THRESHOLD      84
#define MUL_TOOM42_TO_TOOM53_THRESHOLD      81
#define MUL_TOOM42_TO_TOOM63_THRESHOLD      72

#define SQR_BASECASE_THRESHOLD               4
#define SQR_TOOM2_THRESHOLD                 26
#define SQR_TOOM3_THRESHOLD                 87
#define SQR_TOOM4_THRESHOLD                136
#define SQR_TOOM6_THRESHOLD                171
#define SQR_TOOM8_THRESHOLD                246

#define MULMOD_BNM1_THRESHOLD                9
#define SQRMOD_BNM1_THRESHOLD               11

#define MUL_FFT_MODF_THRESHOLD             212  /* k = 5 */
#define MUL_FFT_TABLE3                                      \
  { {    212, 5}, {      9, 6}, {      5, 5}, {     11, 6}, \
    {      6, 5}, {     13, 6}, {     13, 7}, {      7, 6}, \
    {     17, 7}, {     13, 8}, {      7, 7}, {     17, 8}, \
    {      9, 7}, {     19, 8}, {     11, 7}, {     23, 8}, \
    {     13, 9}, {      7, 8}, {     19, 9}, {     11, 8}, \
    {     23,10}, {      7, 9}, {     15, 8}, {     31, 9}, \
    {     19, 8}, {     41, 9}, {     23,10}, {     15, 9}, \
    {     39,10}, {     23, 9}, {     47,11}, {     15,10}, \
    {     31, 9}, {     67,10}, {     39, 9}, {     79,10}, \
    {     47,11}, {   2048,12}, {   4096,13}, {   8192,14}, \
    {  16384,15}, {  32768,16}, {  65536,17}, { 131072,18}, \
    { 262144,19}, { 524288,20}, {1048576,21}, {2097152,22}, \
    {4194304,23}, {8388608,24} }
#define MUL_FFT_TABLE3_SIZE 50
#define MUL_FFT_THRESHOLD                 2240

#define SQR_FFT_MODF_THRESHOLD             184  /* k = 5 */
#define SQR_FFT_TABLE3                                      \
  { {    184, 5}, {     11, 6}, {     13, 7}, {      7, 6}, \
    {     15, 7}, {     13, 8}, {      7, 7}, {     16, 8}, \
    {      9, 7}, {     19, 8}, {     11, 7}, {     23, 8}, \
    {     13, 9}, {      7, 8}, {     19, 9}, {     11, 8}, \
    {     23,10}, {      7, 9}, {     15, 8}, {     31, 9}, \
    {     23,10}, {     15, 9}, {     39,10}, {     23,11}, \
    {     15,10}, {     31, 9}, {     63, 8}, {    127,10}, \
    {     47,11}, {   2048,12}, {   4096,13}, {   8192,14}, \
    {  16384,15}, {  32768,16}, {  65536,17}, { 131072,18}, \
    { 262144,19}, { 524288,20}, {1048576,21}, {2097152,22}, \
    {4194304,23}, {8388608,24} }
#define SQR_FFT_TABLE3_SIZE 42
#define SQR_FFT_THRESHOLD                 1728

#define MULLO_BASECASE_THRESHOLD             2
#define MULLO_DC_THRESHOLD                  45
#define MULLO_MUL_N_THRESHOLD             4392

#define DC_DIV_QR_THRESHOLD                 40
#define DC_DIVAPPR_Q_THRESHOLD             154
#define DC_BDIV_QR_THRESHOLD                42
#define DC_BDIV_Q_THRESHOLD                102

#define INV_MULMOD_BNM1_THRESHOLD           26
#define INV_NEWTON_THRESHOLD               226
#define INV_APPR_THRESHOLD                 171

#define BINV_NEWTON_THRESHOLD              222
#define REDC_1_TO_REDC_N_THRESHOLD          46

#define MU_DIV_QR_THRESHOLD                855
#define MU_DIVAPPR_Q_THRESHOLD             942
#define MUPI_DIV_QR_THRESHOLD               99
#define MU_BDIV_QR_THRESHOLD               680
#define MU_BDIV_Q_THRESHOLD                855

#define MATRIX22_STRASSEN_THRESHOLD         15
#define HGCD_THRESHOLD                      89
#define GCD_DC_THRESHOLD                   273
#define GCDEXT_DC_THRESHOLD                209
#define JACOBI_BASE_METHOD                   2

#define GET_STR_DC_THRESHOLD                32
#define GET_STR_PRECOMPUTE_THRESHOLD        47
#define SET_STR_DC_THRESHOLD               532
#define SET_STR_PRECOMPUTE_THRESHOLD      1336
