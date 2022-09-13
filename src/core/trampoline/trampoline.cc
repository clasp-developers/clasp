
/*
 This file generates trampoline function llvm-IR that can be edited to generate stackmaps.
  
 Compile this file with the following command...


 	clang++-14 -c -emit-llvm -g -O3 -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer trampoline.cc
	llvm-dis-14 trampoline.bc

 Then make the following changes to the trampoline.ll file and insert it into llvmoPackage.cc:

meister@zeus:~/Development/test$ diff -u trampoline.ll trampoline-done.ll 
--- trampoline.ll	2022-09-05 15:58:15.961365240 -0400
+++ trampoline-done.ll	2022-09-05 15:57:47.576558426 -0400
@@ -1,13 +1,19 @@
+
+
 ; ModuleID = 'trampoline.bc'
 source_filename = "trampoline.cc"
 target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
 target triple = "x86_64-pc-linux-gnu"
 
+@__clasp_gcroots_in_module_trampoline = internal global { i64, i8*, i64, i64, i8**, i64 } zeroinitializer
+@__clasp_literals_trampoline = internal global [0 x i8*] zeroinitializer
+
 @_ZL16global_save_args = internal unnamed_addr global i64* null, align 8, !dbg !0
 
 ; Function Attrs: mustprogress uwtable
 define dso_local { i8*, i64 } @interpreter_trampoline_with_stackmap({ i8*, i64 } (i8*, i64, i8**)* nocapture noundef readonly %0, i8* noundef %1, i64 noundef %2, i8** noundef %3) local_unnamed_addr #0 !dbg !104 {
   %5 = alloca [3 x i64], align 16
+  call void (i64, i32, ...) @llvm.experimental.stackmap(i64 3735879680, i32 0, [3 x i64]* nonnull %5)
   call void @llvm.dbg.value(metadata { i8*, i64 } (i8*, i64, i8**)* %0, metadata !117, metadata !DIExpression()), !dbg !125
   call void @llvm.dbg.value(metadata i8* %1, metadata !118, metadata !DIExpression()), !dbg !125
   call void @llvm.dbg.value(metadata i64 %2, metadata !119, metadata !DIExpression()), !dbg !125
@@ -16,7 +22,6 @@
   call void @llvm.lifetime.start.p0i8(i64 24, i8* nonnull %6) #4, !dbg !126
   call void @llvm.dbg.declare(metadata [3 x i64]* %5, metadata !121, metadata !DIExpression()), !dbg !127
   %7 = getelementptr inbounds [3 x i64], [3 x i64]* %5, i64 0, i64 0, !dbg !128
-  store i64* %7, i64** @_ZL16global_save_args, align 8, !dbg !129, !tbaa !130
   %8 = ptrtoint i8* %1 to i64, !dbg !134
   store i64 %8, i64* %7, align 16, !dbg !135, !tbaa !136
   %9 = getelementptr inbounds [3 x i64], [3 x i64]* %5, i64 0, i64 1, !dbg !138
@@ -41,6 +46,7 @@
 ; Function Attrs: mustprogress uwtable
 define dso_local { i8*, i64 } @bytecode_trampoline_with_stackmap({ i8*, i64 } (i64, i8*, i64, i8**)* nocapture noundef readonly %0, i64 noundef %1, i8* noundef %2, i64 noundef %3, i8** noundef %4) local_unnamed_addr #0 !dbg !145 {
   %6 = alloca [3 x i64], align 16
+  call void (i64, i32, ...) @llvm.experimental.stackmap(i64 3735879680, i32 0, [3 x i64]* nonnull %6)
   call void @llvm.dbg.value(metadata { i8*, i64 } (i64, i8*, i64, i8**)* %0, metadata !153, metadata !DIExpression()), !dbg !159
   call void @llvm.dbg.value(metadata i64 %1, metadata !154, metadata !DIExpression()), !dbg !159
   call void @llvm.dbg.value(metadata i8* %2, metadata !155, metadata !DIExpression()), !dbg !159

...

-  store i64* %7, i64** @_ZL16global_save_args, align 8, !dbg !129, !tbaa !130

@@ -64,18 +70,21 @@
 }

 
 ; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone uwtable willreturn
-define dso_local void @CLASP_STARTUP_trampoline() local_unnamed_addr #3 !dbg !173 {
+define dso_local void @"CLASP_STARTUP_trampoline"() local_unnamed_addr #3 !dbg !173 {
   ret void, !dbg !177
 }
 
 ; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
 declare void @llvm.dbg.value(metadata, metadata, metadata) #1
 
+declare void @llvm.experimental.stackmap(i64, i32, ...) #5
+
 attributes #0 = { mustprogress uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
 attributes #1 = { nofree nosync nounwind readnone speculatable willreturn }
 attributes #2 = { argmemonly nofree nosync nounwind willreturn }
 attributes #3 = { mustprogress nofree norecurse nosync nounwind readnone uwtable willreturn "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
 attributes #4 = { nounwind }
+attributes #5 = { nofree nosync willreturn }
 
 !llvm.dbg.cu = !{!2}
 !llvm.module.flags = !{!96, !97, !98, !99, !100, !101, !102}
@@ -259,3 +268,5 @@
 !175 = !{null}
 !176 = !{}
 !177 = !DILocation(line: 42, column: 3, scope: !173)
+
+
*/


#include <cstdint>


struct return_type {
  void* _ptr;
  uint64_t _nvals;
return_type(void* ptr, uint64_t nvals) : _ptr(ptr), _nvals(nvals) {};
};

typedef uint64_t* save_args;

typedef return_type (interpreter_trampoline_type)(void* closure, std::size_t nargs, void** args);
typedef return_type (bytecode_trampoline_type)(uint64_t pc, void* closure, std::size_t nargs, void** args);

static uint64_t* global_save_args;

extern "C" {


  return_type interpreter_trampoline_with_stackmap(interpreter_trampoline_type fn, void* closure, std::size_t nargs, void** args) {
    uint64_t trampoline_save_args[3];
    global_save_args = trampoline_save_args;
    trampoline_save_args[0] = (uintptr_t)closure;
    trampoline_save_args[1] = (uintptr_t)nargs;
    trampoline_save_args[2] = (uintptr_t)args;
    return (fn)(closure,nargs,args);
  }

  return_type bytecode_call( uint64_t pc, void* closure, std::size_t nargs, void** args);

  return_type bytecode_trampoline_with_stackmap( uint64_t pc, void* closure, std::size_t nargs, void** args) {
    uint64_t trampoline_save_args[3];
    global_save_args = trampoline_save_args;
    trampoline_save_args[0] = (uintptr_t)closure;
    trampoline_save_args[1] = (uintptr_t)nargs;
    trampoline_save_args[2] = (uintptr_t)args;
    return bytecode_call(pc,closure,nargs,args);
  }


  void CLASP_STARTUP_trampoline()
  {
  };
};
