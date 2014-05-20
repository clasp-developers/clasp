; ModuleID = 'testDwarf.cc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.8.0"

@.str = private unnamed_addr constant [8 x i8] c"x = %d\0A\00", align 1
@.str1 = private unnamed_addr constant [22 x i8] c"This is a test %d %d\0A\00", align 1

; Function Attrs: ssp uwtable
define i32 @_Z1ai(i32 %x) #0 {
entry:
  %x.addr = alloca i32, align 4
  store i32 %x, i32* %x.addr, align 4
  call void @llvm.dbg.declare(metadata !{i32* %x.addr}, metadata !16), !dbg !17
  %0 = load i32* %x.addr, align 4, !dbg !18
  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([8 x i8]* @.str, i32 0, i32 0), i32 %0), !dbg !18
  %1 = load i32* %x.addr, align 4, !dbg !19
  ret i32 %1, !dbg !19
}

; Function Attrs: nounwind readnone
declare void @llvm.dbg.declare(metadata, metadata) #1

declare i32 @printf(i8*, ...) #2

; Function Attrs: ssp uwtable
define i32 @main(i32 %argc, i8** %argv) #0 {
entry:
  %argc.addr = alloca i32, align 4
  %argv.addr = alloca i8**, align 8
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  store i32 %argc, i32* %argc.addr, align 4
  call void @llvm.dbg.declare(metadata !{i32* %argc.addr}, metadata !20), !dbg !21
  store i8** %argv, i8*** %argv.addr, align 8
  call void @llvm.dbg.declare(metadata !{i8*** %argv.addr}, metadata !22), !dbg !21
  call void @llvm.dbg.declare(metadata !{i32* %x}, metadata !23), !dbg !24
  store i32 1, i32* %x, align 4, !dbg !24
  call void @llvm.dbg.declare(metadata !{i32* %y}, metadata !25), !dbg !27
  store i32 2, i32* %y, align 4, !dbg !27
  %0 = load i32* %x, align 4, !dbg !28
  %1 = load i32* %y, align 4, !dbg !28
  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([22 x i8]* @.str1, i32 0, i32 0), i32 %0, i32 %1), !dbg !28
  %call1 = call i32 @_Z1ai(i32 1), !dbg !31
  ret i32 0, !dbg !32
}

attributes #0 = { ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone }
attributes #2 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!15}

!0 = metadata !{i32 786449,
		metadata !1,
		i32 4,
		metadata !"clang version 3.4 (trunk) (llvm/trunk 192535)",
		i1 false,
		metadata !"",
		i32 0,
		metadata !2,
		metadata !2,
		metadata !3,
		metadata !2,
		metadata !2,
		metadata !""} ; [ DW_TAG_compile_unit ] [/Users/meister/Development/cando/brcl/src/tests/cxx/testDwarf.cc] [DW_LANG_C_plus_plus]
!1 = metadata !{metadata !"testDwarf.cc", metadata !"/Users/meister/Development/cando/brcl/src/tests/cxx"}
!2 = metadata !{i32 0}
!3 = metadata !{metadata !4, metadata !9}
!4 = metadata !{i32 786478,
		metadata !1,
		metadata !5,
		metadata !"a",
		metadata !"a",
		metadata !"a",
		i32 3,
		metadata !6,
		i1 false,
		i1 true,
		i32 0,
		i32 0,
		null,
		i32 256,
		i1 false,
		i32 (i32)* @_Z1ai,
		null,
		null,
		metadata !2,
		i32 4} ; [ DW_TAG_subprogram ] [line 3] [def] [scope 4] [a]
!5 = metadata !{i32 786473, metadata !1}          ; [ DW_TAG_file_type ] [/Users/meister/Development/cando/brcl/src/tests/cxx/testDwarf.cc]
!6 = metadata !{i32 786453,
		i32 0,
		null,
		metadata !"",
		i32 0,
		i64 0,
		i64 0,
		i64 0,
		i32 0,
		null,
		metadata !7,
		i32 0,
		null,
		null,
		null} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!7 = metadata !{metadata !8, metadata !8}
!8 = metadata !{i32 786468, null, null, metadata !"int", i32 0, i64 32, i64 32, i64 0, i32 0, i32 5} ; [ DW_TAG_base_type ] [int] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed]
!9 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"main", metadata !"main", metadata !"", i32 10, metadata !10, i1 false, i1 true, i32 0, i32 0, null, i32 256, i1 false, i32 (i32, i8**)* @main, null, null, metadata !2, i32 11} ; [ DW_TAG_subprogram ] [line 10] [def] [scope 11] [main]
!10 = metadata !{i32 786453, i32 0, null, metadata !"", i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !11, i32 0, null, null, null} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!11 = metadata !{metadata !8, metadata !8, metadata !12}
!12 = metadata !{i32 786447, null, null, metadata !"", i32 0, i64 64, i64 64, i64 0, i32 0, metadata !13} ; [ DW_TAG_pointer_type ] [line 0, size 64, align 64, offset 0] [from ]
!13 = metadata !{i32 786447, null, null, metadata !"", i32 0, i64 64, i64 64, i64 0, i32 0, metadata !14} ; [ DW_TAG_pointer_type ] [line 0, size 64, align 64, offset 0] [from char]
!14 = metadata !{i32 786468, null, null, metadata !"char", i32 0, i64 8, i64 8, i64 0, i32 0, i32 6} ; [ DW_TAG_base_type ] [char] [line 0, size 8, align 8, offset 0, enc DW_ATE_signed_char]
!15 = metadata !{i32 2, metadata !"Dwarf Version", i32 2}
!16 = metadata !{i32 786689, metadata !4, metadata !"x", metadata !5, i32 16777219, metadata !8, i32 0, i32 0} ; [ DW_TAG_arg_variable ] [x] [line 3]
!17 = metadata !{i32 3, i32 0, metadata !4, null}
!18 = metadata !{i32 5, i32 0, metadata !4, null}
!19 = metadata !{i32 6, i32 0, metadata !4, null}
!20 = metadata !{i32 786689, metadata !9, metadata !"argc", metadata !5, i32 16777226, metadata !8, i32 0, i32 0} ; [ DW_TAG_arg_variable ] [argc] [line 10]
!21 = metadata !{i32 10, i32 0, metadata !9, null}
!22 = metadata !{i32 786689, metadata !9, metadata !"argv", metadata !5, i32 33554442, metadata !12, i32 0, i32 0} ; [ DW_TAG_arg_variable ] [argv] [line 10]
!23 = metadata !{i32 786688, metadata !9, metadata !"x", metadata !5, i32 12, metadata !8, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [x] [line 12]
!24 = metadata !{i32 12, i32 0, metadata !9, null}
!25 = metadata !{i32 786688, metadata !26, metadata !"y", metadata !5, i32 14, metadata !8, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [y] [line 14]
!26 = metadata !{i32 786443,
		 metadata !1,
		 metadata !9,
		 i32 13,
		 i32 0,
		 i32 0} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/tests/cxx/testDwarf.cc]
!27 = metadata !{i32 14, i32 0, metadata !26, null}
!28 = metadata !{i32 16, i32 0, metadata !29, null}
!29 = metadata !{i32 786443, metadata !1, metadata !30, i32 15, i32 0, i32 2} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/tests/cxx/testDwarf.cc]
!30 = metadata !{i32 786443, metadata !1, metadata !26, i32 15, i32 0, i32 1} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/tests/cxx/testDwarf.cc]
!31 = metadata !{i32 18, i32 0, metadata !26, null}
!32 = metadata !{i32 20, i32 0, metadata !9, null}
