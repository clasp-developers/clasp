defparameter *a* (make-instance 'xxx))
(defparameter *a* (make-instance 'xxx))
../../src/clbind/constructor.h:127 Allocating instance of Derivable class: XXX
../../src/clbind/constructor.h:129 obj.px_ref() = 0x10f044510
../../src/clbind/constructor.h:130 typeid(obj.px_ref())@0x10315cbd0  typeid(obj.px_ref()).name=PN10asttooling22DerivableMatchCallbackE
../../src/clbind/constructor.h:127 Allocating instance of Derivable class: XXX
../../src/clbind/constructor.h:129 obj.px_ref() = 0x10eabb2e0
../../src/clbind/constructor.h:130 typeid(obj.px_ref())@0x10315cbd0  typeid(obj.px_ref()).name=PN10asttooling22DerivableMatchCallbackE

*A*
> (ast-tooling:test-derivable *a*)
(ast-tooling:test-derivable *a*)
../../src/clbind/clbind_wrappers.h:418 In from_object<T*>(core::T_sp o)
dynamic_cast<clbind::Derivable<T>*>(o.px_ref()) = 0x0 (SHOULD NOT BE NULL!!!)
o.px_ref() = 0x10eabb2e0
typeid(o.px_ref())@0x10313aff0  typeid(o.px_ref()).name=PN4core3T_OE
typeid(clbind::Derivable<T>*)@0x10315cbb0   typeid(clbind::Derivable<T>*).name() = PN6clbind9DerivableIN10asttooling22DerivableMatchCallbackEEE
dynamic_cast<void*>(o.px_ref()) = 0x10eabb2e0
Invoking o.px_ref()->describe(); /* A virtual function */
../../src/asttooling/clangTooling.h:293 Entered DerivableMatchCallback::describe()
this=0x10eabb2e0  typeid(this)@0x10315cbd0  typeid(this).name=PN10asttooling22DerivableMatchCallbackE
dynamic_cast<void*>(this) = 0x10eabb2e0
dynamic_cast<core::T_O*>(this) = 0x10eabb2e0
typeid(dynamic_cast<core::T_O>*>(this))@0x10313aff0  typeid.name=PN4core3T_OE
dynamic_cast<Derivable<clang::ast_matchers::MatchFinder::MatchCallback>*>(this) = 0x10eabb2e0
dynamic_cast<DerivableMatchCallback*>(this) = 0x10eabb2e0
alien pointer = 0x10eabb340
isgf 0
_Class: #<CL:STANDARD-CLASS XXX 0x10f73abd0>
_Slots[0]: unbound


Condition of type: CORE:SIMPLE-PROGRAM-ERROR
Could not convert #<a COMMON-LISP-USER::XXX> of RTTI type N3mem9smart_ptrIN4core3T_OEEE to PN10asttooling22DerivableMatchCallbackE
Available restarts:

1. (CORE::RESTART-TOPLEVEL) Go back to Top-Level REPL.

Broken at frame[13] :ZOMBI.
 File: "-no-file-" (Position #0)
>> 
