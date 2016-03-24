(print "Testing")
(progn
  (require :clang-tool)
  (require :clasp-analyzer)
  (print "Done"))

(in-package :clasp-analyzer)

(defparameter *db*
  (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
   #P"app-resources:build-databases;clasp_compile_commands.json"
   :selection-pattern ".*functor.cc.*"
   ))


(clasp-analyzer:search/generate-code *db*)

(time (clasp-analyzer:load-project *db*))

(print "Done Loading")
(gethash "core::Functor_O" (project-classes *project*))

(setf *print-pretty* nil)


(defparameter *db*
  (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
   #P"app-resources:build-databases;clasp_compile_commands.json"
))
(analyze-only *db*)
(search "bc" "abcdefg")

(maphash (lambda (k v) (when (search "Wrap" k) (print k))) (project-lispallocs *project*))
*project*

(gethash "core::Cons_O" (project-classes *project*))#S(CCLASS :KEY "core::Cons_O" :TEMPLATE-SPECIALIZER NIL :LOCATION "/Users/meister/Development/clasp/include/clasp/core/cons.h:111:1" :BASES ("core::T_O") :VBASES NIL :FIELDS (#S(INSTANCE-VARIABLE :FIELD-NAME "_Cdr" :LOCATION "/Users/meister/Development/clasp/include/clasp/core/cons.h:169:3" :CTYPE #S(SMART-PTR-CTYPE :KEY "gctools::smart_ptr<core::T_O>" :SPECIALIZER "class core::T_O")) #S(INSTANCE-VARIABLE :FIELD-NAME "_Car" :LOCATION "/Users/meister/Development/clasp/include/clasp/core/cons.h:168:3" :CTYPE #S(SMART-PTR-CTYPE :KEY "gctools::smart_ptr<core::T_O>" :SPECIALIZER "class core::T_O"))) :SIZE NIL :METHOD-NAMES ("NO-NAME" "NO-NAME" "NO-NAME" "NO-NAME" "NO-NAME" "NO-NAME" "NO-NAME" "getf" "setf_subseq" "subseq" "eq" "booleanAnd" "booleanOr" "min" "max" "product" "__write__" "__repr__" "describe" "exactlyMatches" "assoc" "member" "member1" "memberEql" "memberEq" "olookupKeyObjectDefault" "olookupKeyObject" "olistrefArgument" "olistref" "fastUnsafeLength" "length" "copyTreeCar" "copyTree" "copyListCar" "copyList" "last" "setf_cdr" "setCdr" "nreconc" "revappend" "nreverse" "reverse" "extend" "filterOutNil" "setf_nth" "equalp" "equal" "setf_car" "setCar" "ocaddr" "ocadr" "ocar" "cdr" "cdrPtr" "setf_elt" "elt" "onthcdr" "onth" "rplacd" "rplaca" "sxhash_" "append" "appendInto" "cdr_offset" "car_offset" "create" "create" "createList" "createList" "createList" "createList" "createList" "createList" "createList" "createList" "createFrom_va_list" "setPad" "setPad1" "fwdPointer" "setFwdPointer" "padSize" "padP" "pad1P" "fwdP" "hasGcTag" "rawRef" "rawRef" "expose_to_clasp" "register_class_with_redeye" "Pkg" "Package" "static_classSymbol" "static_className" "static_packageName" "set_static_creator" "set_static_class_symbol" "asSmartPtr" "asSmartPtr") :METADATA NIL)
T




(progn
  (defparameter *classes* (project-classes *project*))
  (defparameter *analysis* (analyze-project *project*))
  (setq *print-pretty* nil)
  (defparameter *cws* (gethash "core::ClosureWithSlots" *classes*))
  (defparameter *cwr* (gethash "core::ClosureWithRecords" *classes*))
  (defparameter *cc* (gethash "gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>" *classes*))
  )
*cwr*


(trace linearize-code-for-field linearize-class-layout-impl)
(untrace)
(defparameter *ls* (class-layout *cws* *analysis*))
*ls*
(defparameter *lr* (class-layout *cwr* *analysis*))

(defparameter *lc* (class-layout *cc* *analysis*))
*lc*
*lr*
*node*
(codegen-layout t "core__ClosureWithRecords" "core::ClosureWithRecords" *lr*)
(codegen-layout t "core__ClosureWithSlots" "core::ClosureWithSlots" *ls*)
(codegen-container-layout t "foo" "bar" *lc*)
(variable-part *lr*)


(fifth *l*)
(codegen-offsets (fifth *l*))

(trace fixable-instance-variables-impl)
(fixable-instance-variables *cws* *analysis*)

(cclass-fields *cws*)
(first (cclass-fields *cws*))
*cws*
(fixable-instance-variables *cws* *analysis*)
(fixable-instance-variables (instance-variable-ctype (car (third (fixable-instance-variables *cws* *analysis*)))) *analysis*)
(car (third (fixable-instance-variables *cws* *analysis*)))
(class-of (first (cclass-fields *cws*)))
(contains-fixptr-p (first (cclass-fields *cws*)) *project*)
(gethash "core::SlotData" *classes*)
(contains-fixptr-p (gethash "core::SlotData" *classes*) *project*)
(gethash "gctools::GCArray_moveable<core::SlotData,0>" *classes*)

(trace contains-fixptr-impl-p)


(code-for-class-layout (cclass-key *cws*) (class-layout *cws* *analysis*) *project*)
(variable-part (class-layout *cws* *analysis*))
(fixable-instance-variables (car (variable-part (class-layout *cws* *analysis*))) *analysis*)
(defparameter *v* (instance-variable-ctype (car (variable-part (class-layout *cws* *analysis*)))))
(fixable-instance-variables *v* *analysis*)

((class-layout *cws* *analysis*)
(defparameter *analysis* (analyze-project *project*))


(gethash "core::LogicalPathname_O" (analysis-enums *analysis*))
T
