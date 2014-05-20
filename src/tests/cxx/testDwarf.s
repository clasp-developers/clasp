	.section	__TEXT,__text,regular,pure_instructions
	.section	__DWARF,__debug_info,regular,debug
Lsection_info:
	.section	__DWARF,__debug_abbrev,regular,debug
Lsection_abbrev:
	.section	__DWARF,__debug_aranges,regular,debug
	.section	__DWARF,__debug_macinfo,regular,debug
	.section	__DWARF,__debug_line,regular,debug
Lsection_line:
	.section	__DWARF,__debug_loc,regular,debug
	.section	__DWARF,__debug_str,regular,debug
Linfo_string:
	.section	__DWARF,__debug_ranges,regular,debug
Ldebug_range:
	.section	__DWARF,__debug_loc,regular,debug
Lsection_debug_loc:
	.section	__TEXT,__text,regular,pure_instructions
Ltext_begin:
	.section	__DATA,__data
	.file	1 "testDwarf.cc"
	.section	__TEXT,__text,regular,pure_instructions
	.globl	__Z1ai
	.align	4, 0x90
__Z1ai:                                 ## @_Z1ai
	.cfi_startproc
Lfunc_begin0:
	.loc	1 4 0                   ## testDwarf.cc:4:0
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp2:
	.cfi_def_cfa_offset 16
Ltmp3:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp4:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	%edi, %eax
	movl	%eax, -4(%rbp)
	.loc	1 5 0 prologue_end      ## testDwarf.cc:5:0
Ltmp5:
	leaq	L_.str(%rip), %rdi
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	_printf
	.loc	1 6 0                   ## testDwarf.cc:6:0
	movl	-4(%rbp), %eax
	addq	$16, %rsp
	popq	%rbp
	ret
Ltmp6:
Lfunc_end0:
	.cfi_endproc

	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
Lfunc_begin1:
	.loc	1 11 0                  ## testDwarf.cc:11:0
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp9:
	.cfi_def_cfa_offset 16
Ltmp10:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp11:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	.loc	1 12 0 prologue_end     ## testDwarf.cc:12:0
Ltmp12:
	movl	$1, -20(%rbp)
	.loc	1 14 0                  ## testDwarf.cc:14:0
Ltmp13:
	movl	$2, -24(%rbp)
	.loc	1 16 0                  ## testDwarf.cc:16:0
Ltmp14:
	movl	-20(%rbp), %esi
	leaq	L_.str1(%rip), %rdi
	movl	$2, %edx
	xorl	%eax, %eax
	callq	_printf
	movl	$1, %edi
Ltmp15:
	.loc	1 18 0                  ## testDwarf.cc:18:0
	callq	__Z1ai
	xorl	%eax, %eax
Ltmp16:
	.loc	1 20 0                  ## testDwarf.cc:20:0
	addq	$32, %rsp
	popq	%rbp
	ret
Ltmp17:
Lfunc_end1:
	.cfi_endproc

	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"x = %d\n"

L_.str1:                                ## @.str1
	.asciz	"This is a test %d %d\n"

	.section	__TEXT,__text,regular,pure_instructions
Ldebug_end0:
	.section	__DWARF,__debug_str,regular,debug
Linfo_string0:
	.asciz	"clang version 3.4 (trunk) (llvm/trunk 192535)"
Linfo_string1:
	.asciz	"testDwarf.cc"
Linfo_string2:
	.asciz	"/Users/meister/Development/cando/brcl/src/tests/cxx"
Linfo_string3:
	.asciz	"a"
Linfo_string4:
	.asciz	"int"
Linfo_string5:
	.asciz	"main"
Linfo_string6:
	.asciz	"x"
Linfo_string7:
	.asciz	"argc"
Linfo_string8:
	.asciz	"argv"
Linfo_string9:
	.asciz	"char"
Linfo_string10:
	.asciz	"y"
	.section	__DWARF,__debug_info,regular,debug
L__DWARF__debug_info_begin0:
	.long	213                     ## Length of Compilation Unit Info
	.short	2                       ## DWARF version number
Lset0 = L__DWARF__debug_abbrev_begin-Lsection_abbrev ## Offset Into Abbrev. Section
	.long	Lset0
	.byte	8                       ## Address Size (in bytes)
	.byte	1                       ## Abbrev [1] 0xb:0xce DW_TAG_compile_unit
Lset1 = Linfo_string0-Linfo_string      ## DW_AT_producer
	.long	Lset1
	.short	4                       ## DW_AT_language
Lset2 = Linfo_string1-Linfo_string      ## DW_AT_name
	.long	Lset2
	.quad	0                       ## DW_AT_low_pc
	.long	0                       ## DW_AT_stmt_list
Lset3 = Linfo_string2-Linfo_string      ## DW_AT_comp_dir
	.long	Lset3
	.byte	2                       ## Abbrev [2] 0x26:0x31 DW_TAG_subprogram
Lset4 = Linfo_string3-Linfo_string      ## DW_AT_MIPS_linkage_name
	.long	Lset4
Lset5 = Linfo_string3-Linfo_string      ## DW_AT_name
	.long	Lset5
	.byte	1                       ## DW_AT_decl_file
	.byte	3                       ## DW_AT_decl_line
	.long	87                      ## DW_AT_type
	.byte	1                       ## DW_AT_external
	.quad	Lfunc_begin0            ## DW_AT_low_pc
	.quad	Lfunc_end0              ## DW_AT_high_pc
	.byte	1                       ## DW_AT_frame_base
	.byte	86
	.byte	3                       ## Abbrev [3] 0x48:0xe DW_TAG_formal_parameter
Lset6 = Linfo_string6-Linfo_string      ## DW_AT_name
	.long	Lset6
	.byte	1                       ## DW_AT_decl_file
	.byte	3                       ## DW_AT_decl_line
	.long	87                      ## DW_AT_type
	.byte	2                       ## DW_AT_location
	.byte	145
	.byte	124
	.byte	0                       ## End Of Children Mark
	.byte	4                       ## Abbrev [4] 0x57:0x7 DW_TAG_base_type
Lset7 = Linfo_string4-Linfo_string      ## DW_AT_name
	.long	Lset7
	.byte	5                       ## DW_AT_encoding
	.byte	4                       ## DW_AT_byte_size
	.byte	5                       ## Abbrev [5] 0x5e:0x69 DW_TAG_subprogram
Lset8 = Linfo_string5-Linfo_string      ## DW_AT_name
	.long	Lset8
	.byte	1                       ## DW_AT_decl_file
	.byte	10                      ## DW_AT_decl_line
	.long	87                      ## DW_AT_type
	.byte	1                       ## DW_AT_external
	.quad	Lfunc_begin1            ## DW_AT_low_pc
	.quad	Lfunc_end1              ## DW_AT_high_pc
	.byte	1                       ## DW_AT_frame_base
	.byte	86
	.byte	3                       ## Abbrev [3] 0x7c:0xe DW_TAG_formal_parameter
Lset9 = Linfo_string7-Linfo_string      ## DW_AT_name
	.long	Lset9
	.byte	1                       ## DW_AT_decl_file
	.byte	10                      ## DW_AT_decl_line
	.long	87                      ## DW_AT_type
	.byte	2                       ## DW_AT_location
	.byte	145
	.byte	124
	.byte	3                       ## Abbrev [3] 0x8a:0xe DW_TAG_formal_parameter
Lset10 = Linfo_string8-Linfo_string     ## DW_AT_name
	.long	Lset10
	.byte	1                       ## DW_AT_decl_file
	.byte	10                      ## DW_AT_decl_line
	.long	211                     ## DW_AT_type
	.byte	2                       ## DW_AT_location
	.byte	145
	.byte	112
	.byte	6                       ## Abbrev [6] 0x98:0xe DW_TAG_variable
Lset11 = Linfo_string6-Linfo_string     ## DW_AT_name
	.long	Lset11
	.byte	1                       ## DW_AT_decl_file
	.byte	12                      ## DW_AT_decl_line
	.long	87                      ## DW_AT_type
	.byte	2                       ## DW_AT_location
	.byte	145
	.byte	108
	.byte	7                       ## Abbrev [7] 0xa6:0x20 DW_TAG_lexical_block
	.quad	Ltmp13                  ## DW_AT_low_pc
	.quad	Ltmp16                  ## DW_AT_high_pc
	.byte	6                       ## Abbrev [6] 0xb7:0xe DW_TAG_variable
Lset12 = Linfo_string10-Linfo_string    ## DW_AT_name
	.long	Lset12
	.byte	1                       ## DW_AT_decl_file
	.byte	14                      ## DW_AT_decl_line
	.long	87                      ## DW_AT_type
	.byte	2                       ## DW_AT_location
	.byte	145
	.byte	104
	.byte	0                       ## End Of Children Mark
	.byte	0                       ## End Of Children Mark
	.byte	4                       ## Abbrev [4] 0xc7:0x7 DW_TAG_base_type
Lset13 = Linfo_string9-Linfo_string     ## DW_AT_name
	.long	Lset13
	.byte	6                       ## DW_AT_encoding
	.byte	1                       ## DW_AT_byte_size
	.byte	8                       ## Abbrev [8] 0xce:0x5 DW_TAG_pointer_type
	.long	199                     ## DW_AT_type
	.byte	8                       ## Abbrev [8] 0xd3:0x5 DW_TAG_pointer_type
	.long	206                     ## DW_AT_type
	.byte	0                       ## End Of Children Mark
L__DWARF__debug_info_end0:
	.section	__DWARF,__debug_abbrev,regular,debug
L__DWARF__debug_abbrev_begin:
	.byte	1                       ## Abbreviation Code
	.byte	17                      ## DW_TAG_compile_unit
	.byte	1                       ## DW_CHILDREN_yes
	.byte	37                      ## DW_AT_producer
	.byte	14                      ## DW_FORM_strp
	.byte	19                      ## DW_AT_language
	.byte	5                       ## DW_FORM_data2
	.byte	3                       ## DW_AT_name
	.byte	14                      ## DW_FORM_strp
	.byte	17                      ## DW_AT_low_pc
	.byte	1                       ## DW_FORM_addr
	.byte	16                      ## DW_AT_stmt_list
	.byte	6                       ## DW_FORM_data4
	.byte	27                      ## DW_AT_comp_dir
	.byte	14                      ## DW_FORM_strp
	.byte	0                       ## EOM(1)
	.byte	0                       ## EOM(2)
	.byte	2                       ## Abbreviation Code
	.byte	46                      ## DW_TAG_subprogram
	.byte	1                       ## DW_CHILDREN_yes
	.ascii	"\207@"                 ## DW_AT_MIPS_linkage_name
	.byte	14                      ## DW_FORM_strp
	.byte	3                       ## DW_AT_name
	.byte	14                      ## DW_FORM_strp
	.byte	58                      ## DW_AT_decl_file
	.byte	11                      ## DW_FORM_data1
	.byte	59                      ## DW_AT_decl_line
	.byte	11                      ## DW_FORM_data1
	.byte	73                      ## DW_AT_type
	.byte	19                      ## DW_FORM_ref4
	.byte	63                      ## DW_AT_external
	.byte	12                      ## DW_FORM_flag
	.byte	17                      ## DW_AT_low_pc
	.byte	1                       ## DW_FORM_addr
	.byte	18                      ## DW_AT_high_pc
	.byte	1                       ## DW_FORM_addr
	.byte	64                      ## DW_AT_frame_base
	.byte	10                      ## DW_FORM_block1
	.byte	0                       ## EOM(1)
	.byte	0                       ## EOM(2)
	.byte	3                       ## Abbreviation Code
	.byte	5                       ## DW_TAG_formal_parameter
	.byte	0                       ## DW_CHILDREN_no
	.byte	3                       ## DW_AT_name
	.byte	14                      ## DW_FORM_strp
	.byte	58                      ## DW_AT_decl_file
	.byte	11                      ## DW_FORM_data1
	.byte	59                      ## DW_AT_decl_line
	.byte	11                      ## DW_FORM_data1
	.byte	73                      ## DW_AT_type
	.byte	19                      ## DW_FORM_ref4
	.byte	2                       ## DW_AT_location
	.byte	10                      ## DW_FORM_block1
	.byte	0                       ## EOM(1)
	.byte	0                       ## EOM(2)
	.byte	4                       ## Abbreviation Code
	.byte	36                      ## DW_TAG_base_type
	.byte	0                       ## DW_CHILDREN_no
	.byte	3                       ## DW_AT_name
	.byte	14                      ## DW_FORM_strp
	.byte	62                      ## DW_AT_encoding
	.byte	11                      ## DW_FORM_data1
	.byte	11                      ## DW_AT_byte_size
	.byte	11                      ## DW_FORM_data1
	.byte	0                       ## EOM(1)
	.byte	0                       ## EOM(2)
	.byte	5                       ## Abbreviation Code
	.byte	46                      ## DW_TAG_subprogram
	.byte	1                       ## DW_CHILDREN_yes
	.byte	3                       ## DW_AT_name
	.byte	14                      ## DW_FORM_strp
	.byte	58                      ## DW_AT_decl_file
	.byte	11                      ## DW_FORM_data1
	.byte	59                      ## DW_AT_decl_line
	.byte	11                      ## DW_FORM_data1
	.byte	73                      ## DW_AT_type
	.byte	19                      ## DW_FORM_ref4
	.byte	63                      ## DW_AT_external
	.byte	12                      ## DW_FORM_flag
	.byte	17                      ## DW_AT_low_pc
	.byte	1                       ## DW_FORM_addr
	.byte	18                      ## DW_AT_high_pc
	.byte	1                       ## DW_FORM_addr
	.byte	64                      ## DW_AT_frame_base
	.byte	10                      ## DW_FORM_block1
	.byte	0                       ## EOM(1)
	.byte	0                       ## EOM(2)
	.byte	6                       ## Abbreviation Code
	.byte	52                      ## DW_TAG_variable
	.byte	0                       ## DW_CHILDREN_no
	.byte	3                       ## DW_AT_name
	.byte	14                      ## DW_FORM_strp
	.byte	58                      ## DW_AT_decl_file
	.byte	11                      ## DW_FORM_data1
	.byte	59                      ## DW_AT_decl_line
	.byte	11                      ## DW_FORM_data1
	.byte	73                      ## DW_AT_type
	.byte	19                      ## DW_FORM_ref4
	.byte	2                       ## DW_AT_location
	.byte	10                      ## DW_FORM_block1
	.byte	0                       ## EOM(1)
	.byte	0                       ## EOM(2)
	.byte	7                       ## Abbreviation Code
	.byte	11                      ## DW_TAG_lexical_block
	.byte	1                       ## DW_CHILDREN_yes
	.byte	17                      ## DW_AT_low_pc
	.byte	1                       ## DW_FORM_addr
	.byte	18                      ## DW_AT_high_pc
	.byte	1                       ## DW_FORM_addr
	.byte	0                       ## EOM(1)
	.byte	0                       ## EOM(2)
	.byte	8                       ## Abbreviation Code
	.byte	15                      ## DW_TAG_pointer_type
	.byte	0                       ## DW_CHILDREN_no
	.byte	73                      ## DW_AT_type
	.byte	19                      ## DW_FORM_ref4
	.byte	0                       ## EOM(1)
	.byte	0                       ## EOM(2)
	.byte	0                       ## EOM(3)
L__DWARF__debug_abbrev_end:
	.section	__DWARF,__debug_aranges,regular,debug
	.long	44                      ## Length of ARange Set
	.short	2                       ## DWARF Arange version number
Lset14 = L__DWARF__debug_info_begin0-Lsection_info ## Offset Into Debug Info Section
	.long	Lset14
	.byte	8                       ## Address Size (in bytes)
	.byte	0                       ## Segment Size (in bytes)
	.byte	255
	.byte	255
	.byte	255
	.byte	255
	.quad	Lfunc_begin0
Lset15 = Ldebug_end0-Lfunc_begin0
	.quad	Lset15
	.quad	0                       ## ARange terminator
	.quad	0
	.section	__DWARF,__debug_ranges,regular,debug
	.section	__DWARF,__debug_macinfo,regular,debug
	.section	__DWARF,__apple_names,regular,debug
Lnames_begin:
	.long	1212240712              ## Header Magic
	.short	1                       ## Header Version
	.short	0                       ## Header Hash Function
	.long	2                       ## Header Bucket Count
	.long	2                       ## Header Hash Count
	.long	12                      ## Header Data Length
	.long	0                       ## HeaderData Die Offset Base
	.long	1                       ## HeaderData Atom Count
	.short	1                       ## DW_ATOM_die_offset
	.short	6                       ## DW_FORM_data4
	.long	0                       ## Bucket 0
	.long	-1                      ## Bucket 1
	.long	177670                  ## Hash in Bucket 0
	.long	2090499946              ## Hash in Bucket 0
	.long	LNames0-Lnames_begin    ## Offset in Bucket 0
	.long	LNames1-Lnames_begin    ## Offset in Bucket 0
LNames0:
Lset16 = Linfo_string3-Linfo_string     ## a
	.long	Lset16
	.long	1                       ## Num DIEs
	.long	38
	.long	0
LNames1:
Lset17 = Linfo_string5-Linfo_string     ## main
	.long	Lset17
	.long	1                       ## Num DIEs
	.long	94
	.long	0
	.section	__DWARF,__apple_objc,regular,debug
Lobjc_begin:
	.long	1212240712              ## Header Magic
	.short	1                       ## Header Version
	.short	0                       ## Header Hash Function
	.long	1                       ## Header Bucket Count
	.long	0                       ## Header Hash Count
	.long	12                      ## Header Data Length
	.long	0                       ## HeaderData Die Offset Base
	.long	1                       ## HeaderData Atom Count
	.short	1                       ## DW_ATOM_die_offset
	.short	6                       ## DW_FORM_data4
	.long	-1                      ## Bucket 0
	.section	__DWARF,__apple_namespac,regular,debug
Lnamespac_begin:
	.long	1212240712              ## Header Magic
	.short	1                       ## Header Version
	.short	0                       ## Header Hash Function
	.long	1                       ## Header Bucket Count
	.long	0                       ## Header Hash Count
	.long	12                      ## Header Data Length
	.long	0                       ## HeaderData Die Offset Base
	.long	1                       ## HeaderData Atom Count
	.short	1                       ## DW_ATOM_die_offset
	.short	6                       ## DW_FORM_data4
	.long	-1                      ## Bucket 0
	.section	__DWARF,__apple_types,regular,debug
Ltypes_begin:
	.long	1212240712              ## Header Magic
	.short	1                       ## Header Version
	.short	0                       ## Header Hash Function
	.long	2                       ## Header Bucket Count
	.long	2                       ## Header Hash Count
	.long	20                      ## Header Data Length
	.long	0                       ## HeaderData Die Offset Base
	.long	3                       ## HeaderData Atom Count
	.short	1                       ## DW_ATOM_die_offset
	.short	6                       ## DW_FORM_data4
	.short	3                       ## DW_ATOM_die_tag
	.short	5                       ## DW_FORM_data2
	.short	4                       ## DW_ATOM_type_flags
	.short	11                      ## DW_FORM_data1
	.long	0                       ## Bucket 0
	.long	1                       ## Bucket 1
	.long	193495088               ## Hash in Bucket 0
	.long	2090147939              ## Hash in Bucket 1
	.long	Ltypes0-Ltypes_begin    ## Offset in Bucket 0
	.long	Ltypes1-Ltypes_begin    ## Offset in Bucket 1
Ltypes0:
Lset18 = Linfo_string4-Linfo_string     ## int
	.long	Lset18
	.long	1                       ## Num DIEs
	.long	87
	.short	36
	.byte	0
	.long	0
Ltypes1:
Lset19 = Linfo_string9-Linfo_string     ## char
	.long	Lset19
	.long	1                       ## Num DIEs
	.long	199
	.short	36
	.byte	0
	.long	0

.subsections_via_symbols
