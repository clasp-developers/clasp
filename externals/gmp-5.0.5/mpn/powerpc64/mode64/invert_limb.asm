dnl  PowerPC-64 mpn_invert_limb -- Invert a normalized limb.

dnl  Copyright 2004, 2005, 2006, 2008 Free Software Foundation, Inc.

dnl  This file is part of the GNU MP Library.

dnl  The GNU MP Library is free software; you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published
dnl  by the Free Software Foundation; either version 3 of the License, or (at
dnl  your option) any later version.

dnl  The GNU MP Library is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
dnl  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl  License for more details.

dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the GNU MP Library.  If not, see http://www.gnu.org/licenses/.

include(`../config.m4')

C		cycles/limb
C POWER3/PPC630:     ?
C POWER4/PPC970:     75 (including call+ret)

C TODO:
C   * Pair multiply instructions.

ASM_START()
PROLOGUE(mpn_invert_limb)
	LEAL(	r12, approx_tab)

	srdi	r11, r3, 32		C r11 = d >> 32
	rlwinm  r9, r11, 10, 23, 30	C r9 = ((d >> 55) & 0xff) << 1
	lhzx	r0, r12, r9		C load initial approximation
	rldic	r10, r0, 6, 42
	mulld	r8, r10, r10
	sldi	r9, r10, 17
	mulld	r0, r8, r11
	srdi	r0, r0, 31
	subf	r10, r0, r9
	mulld	r8, r10, r10
	sldi	r11, r10, 33
	mulhdu	r0, r8, r3
	sldi	r9, r0, 1
	subf	r10, r9, r11
	sldi	r11, r10, 2
	mulhdu	r0, r10, r10
	mulld	r8, r10, r10
	mulhdu	r10, r8, r3
	mulld	r9, r0, r3
	mulhdu	r0, r0, r3
	addc	r8, r9, r10
	addze	r10, r0
	srdi	r0, r8, 62
	rldimi	r0, r10, 2, 0
	sldi	r9, r8, 2
	subfic	r10, r9, 0
	subfe	r8, r0, r11
	mulhdu	r10, r3, r8
	add	r10, r10, r3
	mulld	r9, r3, r8
	subf	r11, r10, r8
	addi	r0, r10, 1
	addi	r8, r11, -1
	and	r0, r3, r0
	addc	r11, r9, r0
	addze	r10, r10
	addc	r0, r11, r3
	addze	r10, r10
	subf	r3, r10, r8
	blr
EPILOGUE()

DEF_OBJECT(approx_tab)
	.short	1023,1020,1016,1012,1008,1004,1000,996
	.short	992,989,985,981,978,974,970,967
	.short	963,960,956,953,949,946,942,939
	.short	936,932,929,926,923,919,916,913
	.short	910,907,903,900,897,894,891,888
	.short	885,882,879,876,873,870,868,865
	.short	862,859,856,853,851,848,845,842
	.short	840,837,834,832,829,826,824,821
	.short	819,816,814,811,809,806,804,801
	.short	799,796,794,791,789,787,784,782
	.short	780,777,775,773,771,768,766,764
	.short	762,759,757,755,753,751,748,746
	.short	744,742,740,738,736,734,732,730
	.short	728,726,724,722,720,718,716,714
	.short	712,710,708,706,704,702,700,699
	.short	697,695,693,691,689,688,686,684
	.short	682,680,679,677,675,673,672,670
	.short	668,667,665,663,661,660,658,657
	.short	655,653,652,650,648,647,645,644
	.short	642,640,639,637,636,634,633,631
	.short	630,628,627,625,624,622,621,619
	.short	618,616,615,613,612,611,609,608
	.short	606,605,604,602,601,599,598,597
	.short	595,594,593,591,590,589,587,586
	.short	585,583,582,581,579,578,577,576
	.short	574,573,572,571,569,568,567,566
	.short	564,563,562,561,560,558,557,556
	.short	555,554,553,551,550,549,548,547
	.short	546,544,543,542,541,540,539,538
	.short	537,536,534,533,532,531,530,529
	.short	528,527,526,525,524,523,522,521
	.short	520,519,518,517,516,515,514,513
END_OBJECT(approx_tab)
ASM_END()
