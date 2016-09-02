
(require :clasp-analyzer)

;;; Everything
(defparameter *db* (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database #P"lib:compile_commands.json"))

;;; Just the gc_interface.cc file
(defparameter *db*
  (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
   #P"lib:compile_commands.json"
   :selection-pattern ".*gc_interface\.cc.*"))

(time (clasp-analyzer:search/generate-code *db*))


(time (clasp-analyzer:search/generate-code (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database #P"lib:compile_commands.json")))
(print "Testing")


(apropos "code-match-timer")


(print "Testing")
(progn
  (format t "About to require clang-tool and clasp-analyzer~%")
  (require :clang-tool)
  (require :clasp-analyzer)
  (format t "Done~%"))

(in-package :clasp-analyzer)

(defparameter *db*
  (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
   #P"app-resources:build-databases;clasp_compile_commands.json"
   :selection-pattern ".*hashTable.cc.*"))

(clasp-analyzer:search/generate-code *db*)

(time (clasp-analyzer:load-project *db*))




(clang-tool:load-asts *db*)

(getpid)4787


(trace clang-tool::compile-matcher*)
(clang-tool:compile-matcher
 '(:record-decl
     (:bind :whole (:record-decl))))
 
(clang-tool:with-compilation-tool-database *db*
  (clang-tool:match-run-loaded-asts
   '(:cxxrecord-decl)
   :limit 10
   :callback
   (make-instance
    'clang-tool:code-match-callback
    :match-code (lambda (match-info)
                  (let* ((node (clang-tool:mtag-node match-info :whole))
                         (name (cast:get-qualified-name-as-string node))
                         (source-pos (clang-tool:mtag-loc-start match-info :whole)))
                    (cast:dump node)
                    (format t "Name: ~a~%" name)
                    (format t "Source: ~a~%" source-pos))))))


(clang-tool:with-compilation-tool-database *db*
  (clang-tool:match-run-loaded-asts
   '(:record-decl (:bind :whole (:record-decl)))
   :limit 10
   :callback
   (make-instance
    'clang-tool:code-match-callback
    :match-code (lambda (match-info)
                  (let* ((node (clang-tool:mtag-node match-info :whole))
                         (name (cast:get-qualified-name-as-string node))
                         (source-pos (clang-tool:mtag-loc-start match-info :whole)))
                    (cast:dump node)
                    (format t "Name: ~a~%" name)
                    (format t "Source: ~a~%" source-pos))))))


(getpid)1433
(analyze-only *db*)



(defparameter *db*
  (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
   #P"app-resources:build-databases;clasp_compile_commands.json"
   :selection-pattern ".*activationFrame.cc.*"))

(find "aNO-NAME" '("a" "b" "NO-NAME" "c") :test #'string=)
(null (search "NO-NAME" "_PackageName.__r_.__first_.NO-NAME.__l.__cap_"))

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

(gethash "core::Cons_O" (project-classes *project*))




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
