


#if defined(_TARGET_OS_DARWIN)
std::string global_trampoline_datalayout_triple = R"big(
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx12.0.0"
)big";
#elif defined(_TARGET_OS3735879680_LINUX)
# error "Add info for linux"
#else
# error "Add info for os"
#endif

std::string global_trampoline = R"trampoline(

%struct.Gcroots = type { i64, i8*, i64, i64, i8**, i64 }

@__clasp_gcroots_in_module_trampoline = local_unnamed_addr global [0 x %struct.Gcroots] zeroinitializer, align 8, !dbg !0
@__clasp_literals_trampoline = local_unnamed_addr global [0 x i8*] zeroinitializer, align 8, !dbg !12

; Function Attrs: mustprogress ssp uwtable
define { i8*, i64 } @WRAPPER_NAME(i64 noundef %0, i8* noundef %1, i64 noundef %2, i8** noundef %3) local_unnamed_addr #0 !dbg !114 {
  %5 = alloca [3 x i64], align 16
  call void @llvm.dbg.value(metadata i64 %0, metadata !126, metadata !DIExpression()), !dbg !134
  call void @llvm.dbg.value(metadata i8* %1, metadata !127, metadata !DIExpression()), !dbg !134
  call void @llvm.dbg.value(metadata i64 %2, metadata !128, metadata !DIExpression()), !dbg !134
  call void @llvm.dbg.value(metadata i8** %3, metadata !129, metadata !DIExpression()), !dbg !134
  %6 = bitcast [3 x i64]* %5 to i8*, !dbg !135
  call void @llvm.lifetime.start.p0i8(i64 24, i8* nonnull %6) #5, !dbg !135
  call void @llvm.dbg.declare(metadata [3 x i64]* %5, metadata !130, metadata !DIExpression()), !dbg !136
  call void (i64, i32, ...) @llvm.experimental.stackmap(i64 noundef 3735879680, i32 noundef 0, [3 x i64]* noundef nonnull %5), !dbg !137
  %7 = ptrtoint i8* %1 to i64, !dbg !138
  %8 = getelementptr inbounds [3 x i64], [3 x i64]* %5, i64 0, i64 0, !dbg !139
  store i64 %7, i64* %8, align 16, !dbg !140, !tbaa !141
  %9 = getelementptr inbounds [3 x i64], [3 x i64]* %5, i64 0, i64 1, !dbg !145
  store i64 %2, i64* %9, align 8, !dbg !146, !tbaa !141
  %10 = ptrtoint i8** %3 to i64, !dbg !147
  %11 = getelementptr inbounds [3 x i64], [3 x i64]* %5, i64 0, i64 2, !dbg !148
  store i64 %10, i64* %11, align 16, !dbg !149, !tbaa !141
  %12 = call { i8*, i64 } @bytecode_call(i64 noundef %0, i8* noundef %1, i64 noundef %2, i8** noundef %3), !dbg !150
  call void @llvm.lifetime.end.p0i8(i64 24, i8* nonnull %6) #5, !dbg !151
  ret { i8*, i64 } %12, !dbg !151
}

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #2

declare !dbg !152 void @llvm.experimental.stackmap(i64 noundef, i32 noundef, ...) local_unnamed_addr #3

declare !dbg !156 { i8*, i64 } @bytecode_call(i64 noundef, i8* noundef, i64 noundef, i8** noundef) local_unnamed_addr #3

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #2

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { mustprogress ssp uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #1 = { nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { argmemonly nofree nosync nounwind willreturn }
attributes #3 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #4 = { mustprogress nofree norecurse nosync nounwind readnone ssp uwtable willreturn "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #5 = { nounwind }

!llvm.dbg.cu = !{!2}
!llvm.module.flags = !{!107, !108, !109, !110, !111, !112}
!llvm.ident = !{!113}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "__clasp_gcroots_in_module_trampoline", scope: !2, file: !3, line: 100, type: !97, isLocal: false, isDefinition: true)
!2 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !3, producer: "Homebrew clang version 14.0.6", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !4, globals: !11, imports: !18, splitDebugInlining: false, nameTableKind: None, sysroot: "/Library/Developer/CommandLineTools/SDKs/MacOSX12.sdk", sdk: "MacOSX12.sdk")
!3 = !DIFile(filename: "trampoline.cc", directory: "/Users/su-chris/Development/clasp/src/core/trampoline")
!4 = !{!5, !8}
!5 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint64_t", file: !6, line: 31, baseType: !7)
!6 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX12.sdk/usr/include/_types/_uint64_t.h", directory: "")
!7 = !DIBasicType(name: "unsigned long long", size: 64, encoding: DW_ATE_unsigned)
!8 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintptr_t", file: !9, line: 34, baseType: !10)
!9 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX12.sdk/usr/include/sys/_types/_uintptr_t.h", directory: "")
!10 = !DIBasicType(name: "unsigned long", size: 64, encoding: DW_ATE_unsigned)
!11 = !{!0, !12}
!12 = !DIGlobalVariableExpression(var: !13, expr: !DIExpression())
!13 = distinct !DIGlobalVariable(name: "__clasp_literals_trampoline", scope: !2, file: !3, line: 101, type: !14, isLocal: false, isDefinition: true)
!14 = !DICompositeType(tag: DW_TAG_array_type, baseType: !15, elements: !16)
!15 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!16 = !{!17}
!17 = !DISubrange(count: 0)
!18 = !{!19, !26, !30, !34, !38, !42, !46, !50, !51, !54, !56, !58, !60, !62, !64, !66, !68, !70, !72, !74, !76, !78, !80, !82, !84, !90, !91, !94}
!19 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !22, file: !25, line: 152)
!20 = !DINamespace(name: "__1", scope: !21, exportSymbols: true)
!21 = !DINamespace(name: "std", scope: null)
!22 = !DIDerivedType(tag: DW_TAG_typedef, name: "int8_t", file: !23, line: 30, baseType: !24)
!23 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX12.sdk/usr/include/sys/_types/_int8_t.h", directory: "")
!24 = !DIBasicType(name: "signed char", size: 8, encoding: DW_ATE_signed_char)
!25 = !DIFile(filename: "/usr/local/opt/llvm@14/bin/../include/c++/v1/cstdint", directory: "")
!26 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !27, file: !25, line: 153)
!27 = !DIDerivedType(tag: DW_TAG_typedef, name: "int16_t", file: !28, line: 30, baseType: !29)
!28 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX12.sdk/usr/include/sys/_types/_int16_t.h", directory: "")
!29 = !DIBasicType(name: "short", size: 16, encoding: DW_ATE_signed)
!30 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !31, file: !25, line: 154)
!31 = !DIDerivedType(tag: DW_TAG_typedef, name: "int32_t", file: !32, line: 30, baseType: !33)
!32 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX12.sdk/usr/include/sys/_types/_int32_t.h", directory: "")
!33 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!34 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !35, file: !25, line: 155)
!35 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !36, line: 30, baseType: !37)
!36 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX12.sdk/usr/include/sys/_types/_int64_t.h", directory: "")
!37 = !DIBasicType(name: "long long", size: 64, encoding: DW_ATE_signed)
!38 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !39, file: !25, line: 157)
!39 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint8_t", file: !40, line: 31, baseType: !41)
!40 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX12.sdk/usr/include/_types/_uint8_t.h", directory: "")
!41 = !DIBasicType(name: "unsigned char", size: 8, encoding: DW_ATE_unsigned_char)
!42 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !43, file: !25, line: 158)
!43 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint16_t", file: !44, line: 31, baseType: !45)
!44 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX12.sdk/usr/include/_types/_uint16_t.h", directory: "")
!45 = !DIBasicType(name: "unsigned short", size: 16, encoding: DW_ATE_unsigned)
!46 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !47, file: !25, line: 159)
!47 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint32_t", file: !48, line: 31, baseType: !49)
!48 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX12.sdk/usr/include/_types/_uint32_t.h", directory: "")
!49 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!50 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !5, file: !25, line: 160)
!51 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !52, file: !25, line: 162)
!52 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least8_t", file: !53, line: 29, baseType: !22)
!53 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX12.sdk/usr/include/stdint.h", directory: "")
!54 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !55, file: !25, line: 163)
!55 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least16_t", file: !53, line: 30, baseType: !27)
!56 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !57, file: !25, line: 164)
!57 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least32_t", file: !53, line: 31, baseType: !31)
!58 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !59, file: !25, line: 165)
!59 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least64_t", file: !53, line: 32, baseType: !35)
!60 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !61, file: !25, line: 167)
!61 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least8_t", file: !53, line: 33, baseType: !39)
!62 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !63, file: !25, line: 168)
!63 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least16_t", file: !53, line: 34, baseType: !43)
!64 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !65, file: !25, line: 169)
!65 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least32_t", file: !53, line: 35, baseType: !47)
!66 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !67, file: !25, line: 170)
!67 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least64_t", file: !53, line: 36, baseType: !5)
!68 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !69, file: !25, line: 172)
!69 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast8_t", file: !53, line: 40, baseType: !22)
!70 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !71, file: !25, line: 173)
!71 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast16_t", file: !53, line: 41, baseType: !27)
!72 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !73, file: !25, line: 174)
!73 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast32_t", file: !53, line: 42, baseType: !31)
!74 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !75, file: !25, line: 175)
!75 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast64_t", file: !53, line: 43, baseType: !35)
!76 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !77, file: !25, line: 177)
!77 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast8_t", file: !53, line: 44, baseType: !39)
!78 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !79, file: !25, line: 178)
!79 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast16_t", file: !53, line: 45, baseType: !43)
!80 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !81, file: !25, line: 179)
!81 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast32_t", file: !53, line: 46, baseType: !47)
!82 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !83, file: !25, line: 180)
!83 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast64_t", file: !53, line: 47, baseType: !5)
!84 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !85, file: !25, line: 182)
!85 = !DIDerivedType(tag: DW_TAG_typedef, name: "intptr_t", file: !86, line: 32, baseType: !87)
!86 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX12.sdk/usr/include/sys/_types/_intptr_t.h", directory: "")
!87 = !DIDerivedType(tag: DW_TAG_typedef, name: "__darwin_intptr_t", file: !88, line: 51, baseType: !89)
!88 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX12.sdk/usr/include/i386/_types.h", directory: "")
!89 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!90 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !8, file: !25, line: 183)
!91 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !92, file: !25, line: 185)
!92 = !DIDerivedType(tag: DW_TAG_typedef, name: "intmax_t", file: !93, line: 32, baseType: !89)
!93 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX12.sdk/usr/include/_types/_intmax_t.h", directory: "")
!94 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !20, entity: !95, file: !25, line: 186)
!95 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintmax_t", file: !96, line: 32, baseType: !10)
!96 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX12.sdk/usr/include/_types/_uintmax_t.h", directory: "")
!97 = !DICompositeType(tag: DW_TAG_array_type, baseType: !98, elements: !16)
!98 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "Gcroots", file: !3, line: 79, size: 384, flags: DIFlagTypePassByValue, elements: !99, identifier: "_ZTS7Gcroots")
!99 = !{!100, !101, !102, !103, !104, !106}
!100 = !DIDerivedType(tag: DW_TAG_member, name: "val1", scope: !98, file: !3, line: 80, baseType: !5, size: 64)
!101 = !DIDerivedType(tag: DW_TAG_member, name: "val2", scope: !98, file: !3, line: 81, baseType: !15, size: 64, offset: 64)
!102 = !DIDerivedType(tag: DW_TAG_member, name: "val3", scope: !98, file: !3, line: 82, baseType: !5, size: 64, offset: 128)
!103 = !DIDerivedType(tag: DW_TAG_member, name: "val4", scope: !98, file: !3, line: 83, baseType: !5, size: 64, offset: 192)
!104 = !DIDerivedType(tag: DW_TAG_member, name: "val5", scope: !98, file: !3, line: 84, baseType: !105, size: 64, offset: 256)
!105 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !15, size: 64)
!106 = !DIDerivedType(tag: DW_TAG_member, name: "val6", scope: !98, file: !3, line: 85, baseType: !5, size: 64, offset: 320)
!107 = !{i32 7, !"Dwarf Version", i32 4}
!108 = !{i32 2, !"Debug Info Version", i32 3}
!109 = !{i32 1, !"wchar_size", i32 4}
!110 = !{i32 7, !"PIC Level", i32 2}
!111 = !{i32 7, !"uwtable", i32 1}
!112 = !{i32 7, !"frame-pointer", i32 2}
!113 = !{!"Homebrew clang version 14.0.6"}
!114 = distinct !DISubprogram(name: "WRAPPER_NAME", scope: !3, file: !3, line: 107, type: !115, scopeLine: 107, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !125)
!115 = !DISubroutineType(types: !116)
!116 = !{!117, !5, !15, !5, !105}
!117 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "return_type", file: !3, line: 88, size: 128, flags: DIFlagTypePassByValue | DIFlagNonTrivial, elements: !118, identifier: "_ZTS11return_type")
!118 = !{!119, !120, !121}
!119 = !DIDerivedType(tag: DW_TAG_member, name: "_ptr", scope: !117, file: !3, line: 89, baseType: !15, size: 64)
!120 = !DIDerivedType(tag: DW_TAG_member, name: "_nvals", scope: !117, file: !3, line: 90, baseType: !5, size: 64, offset: 64)
!121 = !DISubprogram(name: "return_type", scope: !117, file: !3, line: 91, type: !122, scopeLine: 91, flags: DIFlagPrototyped, spFlags: DISPFlagOptimized)
!122 = !DISubroutineType(types: !123)
!123 = !{null, !124, !15, !5}
!124 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !117, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!125 = !{!126, !127, !128, !129, !130}
!126 = !DILocalVariable(name: "pc", arg: 1, scope: !114, file: !3, line: 107, type: !5)
!127 = !DILocalVariable(name: "closure", arg: 2, scope: !114, file: !3, line: 107, type: !15)
!128 = !DILocalVariable(name: "nargs", arg: 3, scope: !114, file: !3, line: 107, type: !5)
!129 = !DILocalVariable(name: "args", arg: 4, scope: !114, file: !3, line: 107, type: !105)
!130 = !DILocalVariable(name: "trampoline_save_args", scope: !114, file: !3, line: 108, type: !131)
!131 = !DICompositeType(tag: DW_TAG_array_type, baseType: !5, size: 192, elements: !132)
!132 = !{!133}
!133 = !DISubrange(count: 3)
!134 = !DILocation(line: 0, scope: !114)
!135 = !DILocation(line: 108, column: 5, scope: !114)
!136 = !DILocation(line: 108, column: 14, scope: !114)
!137 = !DILocation(line: 109, column: 5, scope: !114)
!138 = !DILocation(line: 110, column: 31, scope: !114)
!139 = !DILocation(line: 110, column: 5, scope: !114)
!140 = !DILocation(line: 110, column: 29, scope: !114)
!141 = !{!142, !142, i64 0}
!142 = !{!"long long", !143, i64 0}
!143 = !{!"omnipotent char", !144, i64 0}
!144 = !{!"Simple C++ TBAA"}
!145 = !DILocation(line: 111, column: 5, scope: !114)
!146 = !DILocation(line: 111, column: 29, scope: !114)
!147 = !DILocation(line: 112, column: 31, scope: !114)
!148 = !DILocation(line: 112, column: 5, scope: !114)
!149 = !DILocation(line: 112, column: 29, scope: !114)
!150 = !DILocation(line: 113, column: 12, scope: !114)
!151 = !DILocation(line: 114, column: 3, scope: !114)
!152 = !DISubprogram(name: "llvm.experimental.stackmap", scope: !3, file: !3, line: 103, type: !153, flags: DIFlagPrototyped, spFlags: DISPFlagOptimized, retainedNodes: !155)
!153 = !DISubroutineType(types: !154)
!154 = !{null, !5, !47, null}
!155 = !{}
!156 = !DISubprogram(name: "bytecode_call", scope: !3, file: !3, line: 105, type: !115, flags: DIFlagPrototyped, spFlags: DISPFlagOptimized, retainedNodes: !155)
!157 = distinct !DISubprogram(name: "CLASP_STARTUP", scope: !3, file: !3, line: 117, type: !158, scopeLine: 118, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !155)
!158 = !DISubroutineType(types: !159)
!159 = !{null}
!160 = !DILocation(line: 119, column: 3, scope: !157)

)trampoline";
