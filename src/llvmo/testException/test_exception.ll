; ModuleID = '<stdin>'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.8.0"

%class.XX = type { i32 (...)** }

@.str = private unnamed_addr constant [7 x i8] c" in z\0A\00", align 1
@_ZTIi = external constant i8*
@.str1 = private unnamed_addr constant [6 x i8] c"in a\0A\00", align 1
@.str2 = private unnamed_addr constant [6 x i8] c"In b\0A\00", align 1
@.str3 = private unnamed_addr constant [6 x i8] c"In c\0A\00", align 1
@.str4 = private unnamed_addr constant [12 x i8] c"Do nothing\0A\00", align 1
@_ZTV2XX = linkonce_odr unnamed_addr constant [4 x i8*] [i8* null, i8* bitcast ({ i8*, i8* }* @_ZTI2XX to i8*), i8* bitcast (void (%class.XX*)* @_ZN2XXD1Ev to i8*), i8* bitcast (void (%class.XX*)* @_ZN2XXD0Ev to i8*)]
@.str5 = private unnamed_addr constant [9 x i8] c"Exiting\0A\00", align 1
@_ZTVN10__cxxabiv117__class_type_infoE = external global i8*
@_ZTS2XX = linkonce_odr constant [4 x i8] c"2XX\00"
@_ZTI2XX = linkonce_odr unnamed_addr constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([4 x i8]* @_ZTS2XX, i32 0, i32 0) }

define void @z() uwtable ssp {
entry:
  %xx = alloca i32, align 4
  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @.str, i32 0, i32 0))
  store i32 1, i32* %xx, align 4
  %exception = call i8* @__cxa_allocate_exception(i64 4) nounwind
  %0 = bitcast i8* %exception to i32*
  %1 = load i32* %xx, align 4
  store i32 %1, i32* %0
  call void @__cxa_throw(i8* %exception, i8* bitcast (i8** @_ZTIi to i8*), i8* null) noreturn
  unreachable

return:                                           ; No predecessors!
  ret void
}

declare i32 @printf(i8*, ...)

declare i8* @__cxa_allocate_exception(i64)

declare void @__cxa_throw(i8*, i8*, i8*)

define void @a() uwtable ssp {
entry:
  %xx = alloca %class.XX, align 8
  %exn.slot = alloca i8*
  %ehselector.slot = alloca i32
  call void @_ZN2XXC1Ev(%class.XX* %xx)
  %call = invoke i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([6 x i8]* @.str1, i32 0, i32 0))
          to label %invoke.cont unwind label %lpad

invoke.cont:                                      ; preds = %entry
  invoke void @z()
          to label %invoke.cont1 unwind label %lpad

invoke.cont1:                                     ; preds = %invoke.cont
  call void @_ZN2XXD1Ev(%class.XX* %xx)
  ret void

lpad:                                             ; preds = %invoke.cont, %entry
  %0 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %1 = extractvalue { i8*, i32 } %0, 0
  store i8* %1, i8** %exn.slot
  %2 = extractvalue { i8*, i32 } %0, 1
  store i32 %2, i32* %ehselector.slot
  invoke void @_ZN2XXD1Ev(%class.XX* %xx)
          to label %invoke.cont2 unwind label %terminate.lpad

invoke.cont2:                                     ; preds = %lpad
  br label %eh.resume

eh.resume:                                        ; preds = %invoke.cont2
  %exn = load i8** %exn.slot
  %sel = load i32* %ehselector.slot
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn, 0
  %lpad.val3 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1
  resume { i8*, i32 } %lpad.val3

terminate.lpad:                                   ; preds = %lpad
  %3 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  call void @_ZSt9terminatev() noreturn nounwind
  unreachable
}

define linkonce_odr void @_ZN2XXC1Ev(%class.XX* %this) unnamed_addr uwtable ssp align 2 {
entry:
  %this.addr = alloca %class.XX*, align 8
  store %class.XX* %this, %class.XX** %this.addr, align 8
  %this1 = load %class.XX** %this.addr
  call void @_ZN2XXC2Ev(%class.XX* %this1)
  ret void
}

declare i32 @__gxx_personality_v0(...)

define linkonce_odr void @_ZN2XXD1Ev(%class.XX* %this) unnamed_addr uwtable ssp align 2 {
entry:
  %this.addr = alloca %class.XX*, align 8
  store %class.XX* %this, %class.XX** %this.addr, align 8
  %this1 = load %class.XX** %this.addr
  call void @_ZN2XXD2Ev(%class.XX* %this1)
  ret void
}

declare void @_ZSt9terminatev()

define void @b() uwtable ssp {
entry:
  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([6 x i8]* @.str2, i32 0, i32 0))
  ret void
}

define void @c() uwtable ssp {
entry:
  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([6 x i8]* @.str3, i32 0, i32 0))
  ret void
}

define void @proto_unwind_protect() uwtable ssp {
entry:
  %xx = alloca %class.XX, align 8
  %exn.slot = alloca i8*
  %ehselector.slot = alloca i32
  call void @_ZN2XXC1Ev(%class.XX* %xx)
  invoke void @a()
          to label %invoke.cont unwind label %lpad

invoke.cont:                                      ; preds = %entry
  br label %try.cont

lpad:                                             ; preds = %entry
  %0 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  %1 = extractvalue { i8*, i32 } %0, 0
  store i8* %1, i8** %exn.slot
  %2 = extractvalue { i8*, i32 } %0, 1
  store i32 %2, i32* %ehselector.slot
  br label %catch

catch:                                            ; preds = %lpad
  %exn = load i8** %exn.slot
  %3 = call i8* @__cxa_begin_catch(i8* %exn) nounwind
  invoke void @c()
          to label %invoke.cont2 unwind label %lpad1

invoke.cont2:                                     ; preds = %catch
  invoke void @__cxa_end_catch()
          to label %invoke.cont4 unwind label %lpad3

invoke.cont4:                                     ; preds = %invoke.cont2
  br label %try.cont

try.cont:                                         ; preds = %invoke.cont4, %invoke.cont
  call void @_ZN2XXD1Ev(%class.XX* %xx)
  ret void

lpad1:                                            ; preds = %catch
  %4 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %5 = extractvalue { i8*, i32 } %4, 0
  store i8* %5, i8** %exn.slot
  %6 = extractvalue { i8*, i32 } %4, 1
  store i32 %6, i32* %ehselector.slot
  invoke void @__cxa_end_catch()
          to label %invoke.cont5 unwind label %terminate.lpad

lpad3:                                            ; preds = %invoke.cont2
  %7 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %8 = extractvalue { i8*, i32 } %7, 0
  store i8* %8, i8** %exn.slot
  %9 = extractvalue { i8*, i32 } %7, 1
  store i32 %9, i32* %ehselector.slot
  br label %ehcleanup

invoke.cont5:                                     ; preds = %lpad1
  br label %ehcleanup

ehcleanup:                                        ; preds = %invoke.cont5, %lpad3
  invoke void @_ZN2XXD1Ev(%class.XX* %xx)
          to label %invoke.cont6 unwind label %terminate.lpad

invoke.cont6:                                     ; preds = %ehcleanup
  br label %eh.resume

eh.resume:                                        ; preds = %invoke.cont6
  %exn7 = load i8** %exn.slot
  %sel = load i32* %ehselector.slot
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn7, 0
  %lpad.val8 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1
  resume { i8*, i32 } %lpad.val8

terminate.lpad:                                   ; preds = %ehcleanup, %lpad1
  %10 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  call void @_ZSt9terminatev() noreturn nounwind
  unreachable
}

declare i8* @__cxa_begin_catch(i8*)

declare void @__cxa_end_catch()

define void @proto_cleanup() uwtable ssp {
entry:
  %xx = alloca %class.XX, align 8
  %exn.slot = alloca i8*
  %ehselector.slot = alloca i32
  call void @_ZN2XXC1Ev(%class.XX* %xx)
  invoke void @a()
          to label %invoke.cont unwind label %lpad

invoke.cont:                                      ; preds = %entry
  call void @_ZN2XXD1Ev(%class.XX* %xx)
  ret void

lpad:                                             ; preds = %entry
  %0 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %1 = extractvalue { i8*, i32 } %0, 0
  store i8* %1, i8** %exn.slot
  %2 = extractvalue { i8*, i32 } %0, 1
  store i32 %2, i32* %ehselector.slot
  invoke void @_ZN2XXD1Ev(%class.XX* %xx)
          to label %invoke.cont1 unwind label %terminate.lpad

invoke.cont1:                                     ; preds = %lpad
  br label %eh.resume

eh.resume:                                        ; preds = %invoke.cont1
  %exn = load i8** %exn.slot
  %sel = load i32* %ehselector.slot
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn, 0
  %lpad.val2 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1
  resume { i8*, i32 } %lpad.val2

terminate.lpad:                                   ; preds = %lpad
  %3 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  call void @_ZSt9terminatev() noreturn nounwind
  unreachable
}

define i32 @main(i32 %argc, i8** %argv) uwtable ssp {
entry:
  %argc.addr = alloca i32, align 4
  %argv.addr = alloca i8**, align 8
  store i32 %argc, i32* %argc.addr, align 4
  store i8** %argv, i8*** %argv.addr, align 8
  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str4, i32 0, i32 0))
  ret i32 0
}

define linkonce_odr void @_ZN2XXD2Ev(%class.XX* %this) unnamed_addr uwtable ssp align 2 {
entry:
  %this.addr = alloca %class.XX*, align 8
  store %class.XX* %this, %class.XX** %this.addr, align 8
  %this1 = load %class.XX** %this.addr
  %0 = bitcast %class.XX* %this1 to i8***
  store i8** getelementptr inbounds ([4 x i8*]* @_ZTV2XX, i64 0, i64 2), i8*** %0
  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([9 x i8]* @.str5, i32 0, i32 0))
  ret void
}

define linkonce_odr void @_ZN2XXD0Ev(%class.XX* %this) unnamed_addr uwtable ssp align 2 {
entry:
  %this.addr = alloca %class.XX*, align 8
  %exn.slot = alloca i8*
  %ehselector.slot = alloca i32
  store %class.XX* %this, %class.XX** %this.addr, align 8
  %this1 = load %class.XX** %this.addr
  invoke void @_ZN2XXD1Ev(%class.XX* %this1)
          to label %invoke.cont unwind label %lpad

invoke.cont:                                      ; preds = %entry
  %0 = bitcast %class.XX* %this1 to i8*
  call void @_ZdlPv(i8* %0) nounwind
  ret void

lpad:                                             ; preds = %entry
  %1 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  %2 = extractvalue { i8*, i32 } %1, 0
  store i8* %2, i8** %exn.slot
  %3 = extractvalue { i8*, i32 } %1, 1
  store i32 %3, i32* %ehselector.slot
  %4 = bitcast %class.XX* %this1 to i8*
  call void @_ZdlPv(i8* %4) nounwind
  br label %eh.resume

eh.resume:                                        ; preds = %lpad
  %exn = load i8** %exn.slot
  %sel = load i32* %ehselector.slot
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn, 0
  %lpad.val2 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1
  resume { i8*, i32 } %lpad.val2
}

declare void @_ZdlPv(i8*) nounwind

define linkonce_odr void @_ZN2XXC2Ev(%class.XX* %this) unnamed_addr nounwind uwtable ssp align 2 {
entry:
  %this.addr = alloca %class.XX*, align 8
  store %class.XX* %this, %class.XX** %this.addr, align 8
  %this1 = load %class.XX** %this.addr
  %0 = bitcast %class.XX* %this1 to i8***
  store i8** getelementptr inbounds ([4 x i8*]* @_ZTV2XX, i64 0, i64 2), i8*** %0
  ret void
}
