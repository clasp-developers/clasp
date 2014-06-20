################################################################################
##
#	Name:		hpm_rf	(64 bit)
#	Version:	1.2
#
#	Purpose:	read hardware performance registers for analysis
#	Author:         Rainer Keller extending/cleanup code from
#			Rudolf Fischer
#			NEC European Supercomputer Systems
#	Hardware:	NEC SX-8
#
#	Compilation:
#		as -m -E hpm_rf.s (for float0 8-byte)
#		as -m -F hpm_rf.s (for float2       )
#		currently not working for float0 4-byte
#
#	Usage:	
#		CALL hpm_rf( iarray )
#
#	on return the array iarray 
#	will contain the following integer data:
#
#	 1.	stm	system timer register
#	 2.	usrcc	user clock counter
#	 3.	ex	execution counter
#	 4.	vx	vector execution counter
#	 5.	ve	vector element counter
#	 6.	vecc	vector execution clock counter
#	 7.	varec	vector arithmetic execution clock counter
#	 8.	vldec	vector load execution clock counter
#	 9.	fpec	floating point data execution counter
#	10.	bccc	bank conflict clock counter
#	11.	icmcc	instruction cache miss clock counter
#	12.	ocmcc	operand cache miss clock counter
#	13.	iphcc	instruction pipeline hold clock counter
#	14.	mnccc	memory network conflict clock counter
#       15.	sracc   shared resource access clock counter
#       16.	brec    branch execution counter
#	17.	bpfc	branch prediction failure counter
#
#	Not supported on SX5/6/8
#	# 13.	spec	scalar pipeline execution counter
#
##
################################################################################
	global	hpm_rf_
	text
	llong	0xffffffffffffffff
	long	512
	long	0x00000019
	long 	0,0,0,0,0,0,0,0,0,0,0
	long 	6
	str	"hpm_rf"
	rstr	26," "
#
# Entry, argument and exit registers:
#
define(return,$s32)
define(entry,$s33)
define(arglist,$s34)
#
# Addresses of arguments:
#
define(a_ptr,$s40)
#
# Scalar arguments:
#
#
# Other scalars:
#
#
# Vector Data Registers:
#
#
# Entry point:
#
hpm_rf_:
	using	hpm_rf_,entry
	ldm	a_ptr,1,8(,arglist)	# load all arg. addresses
	ststm	$s50
	sts	$s50,0(,a_ptr)
	stusrcc	$s51
	sts	$s51,8(,a_ptr)
	sex	$s52
	sts	$s52,16(,a_ptr)
	svx	$s53
	sts	$s53,24(,a_ptr)
	sve	$s54
	sts	$s54,32(,a_ptr)
	stvecc	$s55
	sts	$s55,40(,a_ptr)
	stvarec	$s56
	sts	$s56,48(,a_ptr)
	stvldec	$s57
	sts	$s57,56(,a_ptr)
	stfpec	$s50
	sts	$s50,64(,a_ptr)
	stbccc	$s51
	sts	$s51,72(,a_ptr)
	sticmcc	$s52
	sts	$s52,80(,a_ptr)
	stocmcc	$s53
	sts	$s53,88(,a_ptr)
	stiphcc	$s54
	sts	$s54,96(,a_ptr)
	stmnccc	$s55
	sts	$s55,104(,a_ptr)
	stsracc	$s56
	sts	$s56,112(,a_ptr)
	stbrec	$s57
	sts	$s57,120(,a_ptr)
	stbpfc	$s58
	sts	$s58,128(,a_ptr)
	b	0(,return)
