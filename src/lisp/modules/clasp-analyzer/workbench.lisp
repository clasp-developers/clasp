(progn
  (compile-file #P"sys:modules;clang-tool;clang-tool.lisp" :print t)
  (compile-file #P"sys:modules;clasp-analyzer;clasp-analyzer.lisp" :print t)
  (format t "Done compile~%"))
(progn
  (load #P"sys:modules;clang-tool;clang-tool.fasl")
  (load #P"sys:modules;clasp-analyzer;clasp-analyzer.fasl")
  (format t "Done Loading clasp-analyzer~%"))
(in-package :clasp-analyzer)
;;; ------------------------------------------------------------
;;; To load and analyze one file in the project
(progn
  (defparameter *compile-commands* (probe-file "source-dir:build;mpsprep;compile_commands.json"))
  (defvar *db* (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
                (pathname *compile-commands*)
;;                :selection-pattern "lisp.cc"
                ))
  (time (clasp-analyzer:serial-search/generate-code *db*))
  (format t "Done searching project~%"))))


;;; ------------------------------------------------------------
;;; To load and analyze the entire project
(progn
  (defparameter *compile-commands* (probe-file "~/aws/static-analyze-clasp/results/compile_commands.json"))
  (defvar *db* (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
                (pathname *compile-commands*)
                :selection-pattern "bignum.cc"
                ))
  (time
   (progn
     (format t "Loading project~%")
     (defparameter *p1* (load-project *db* "~/aws/static-analyze-clasp/results/project.dat"))
     (format t "Done loading project~%"))))
(progn
  (defparameter *analysis* (analyze-project *p1*))
  (generate-code *analysis* :output-file #P"/tmp/clasp_gc.cc")
  (format t "Done analyze and generate-code for project~%")))


(trace codegen-variable-part)
(defparameter *analysis* (analyze-project *p1*))


(defparameter *bv* (gethash "core::SimpleBitVector_O" (project-classes *project*)))



(trace linearize-class-layout-impl)
(class-layout *bv* *analysis*)






(probe-file "~/slime/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Run a small test search on AWS 
;;;

(trace classify-ctype)
(progn
  (defparameter *compile-commands* (probe-file "/Development/clasp/build/mpsprep/compile_commands.json"))
  (defvar *db* (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
                (pathname *compile-commands*)
                :selection-pattern "/array.cc"))
  (format t "Done setting ctd~%"))

(progn
  (defvar *p* (clasp-analyzer::serial-search/generate-code *db* :output-file #P"/tmp/data.dat"))
  (format t "Done search!~%"))

(defparameter *analysis* (analyze-project *p*))

(defparameter *bv* (gethash "core::SimpleBitVector_O" (project-classes *project*)))

(trace linearize-class-layout-impl)
(class-layout *bv* *analysis*)

*bv*
(maphash (lambda (k v) (if (search "SimpleBitVector" k) (print k))) (project-classes *project*))


(gethash (gethash "core::DerivableCxxObject_O" (analysis-stamps *analysis*)) (analysis-stamp-children *analysis*))

(analysis-stamp-roots *analysis*)
("clang::ast_matchers::MatchFinder::MatchCallback"
 "core::Lisp_O"
 "clang::RecursiveASTVisitor<asttooling::AstVisitor_O>"
 "gctools::GCContainer"
 "clang::FrontendAction"
 "clang::tooling::ToolAction"
 "core::T_O"
 "clbind::detail::class_map")

(children-of-class-named  *analysis*)q

(gethash "core::Array_O" (project-classes *project*))

(analyze-only *db*)



(defvar *compile-commands* "~/Development/clasp/build/mpsprep/compile_commands.json")
(defvar *compile-commands* "~/Development/clasp/build/boehm/compile_commands.json")

(setf *print-pretty* nil)
(defvar *db* (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
              (pathname *compile-commands*)
              :selection-pattern "gc_interface.cc" ))
(defvar *p* (serial-search/generate-code *db* :output-file #P"/tmp/data.dat"))


;;; ------------------------------------------------------------
;;;
;;; Load an existing project
;;;
(defvar *compile-commands* "~/Development/clasp/compile_commands.json")
(defvar *db* (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
              (pathname *compile-commands*)))
(progn
  (defparameter *p1* (load-project "~/Development/clasp/project.dat"))
  (format t "Done read project~%"))

(progn
  (defparameter *analysis* (analyze-project *p1*))
  (generate-code *analysis* :output-file #P"/tmp/clasp_gc.cc")
  (format t "Done analyze and generate-code for project~%"))


(defparameter *a* (gethash "core::Array_O" (project-classes *project*)))

(analysis-stamps *analysis*)
(stamp-value (hierarchy-end-value "core::Array_O" *analysis*))


(analysis-enum-roots *analysis*)
*analysis*
(analyze-only *db*)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test parallel stuff
;;;

(progn
  (compile-file #P"sys:modules;clang-tool;clang-tool.lisp" :print t)
  (compile-file #P"sys:modules;clasp-analyzer;clasp-analyzer.lisp" :print t)
  (format t "Done compile~%"))

(progn
  (load #P"sys:modules;clang-tool;clang-tool.fasl")
  (load #P"sys:modules;clasp-analyzer;clasp-analyzer.fasl"))

(progn
  (defparameter *compile-commands* (probe-file "~/Development/clasp/build/mpsprep/compile_commands.json"))
  (setf *print-pretty* nil)
  (defvar *db* (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
                (pathname *compile-commands*))))

(clasp-analyzer::parallel-search/generate-code *db* :pjobs 6)

(ext:getenv "PJOBS")
(clasp-analyzer::parallel-search-all *db* :jobs 8)

(defparameter *q* (make-cxx-object 'mp:quueue

;;
;; Small run
;;
(load #P"sys:modules;clang-tool;clang-tool.fasl")
(load #P"sys:modules;clasp-analyzer;clasp-analyzer.fasl")

(in-package :clasp-analyzer)
(defvar *compile-commands* "/Users/meister/Development/clasp/build/mpsprep/compile_commands_small.json")
(setf *print-pretty* nil)
(defvar *db* (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database (pathname *compile-commands*)))
(defvar *p* (search/generate-code *db*))







;; Search only
(clang-tool:with-compilation-tool-database *db*
  (setf *p* (serial-search-all *db*)))
;; Search and generate code
(defvar *p* (search/generate-code *db*))
;; Analyze project
(defparameter *a* (analyze-project *p*))
(clang-tool:with-compilation-tool-database *db*
  (generate-code *a*))










(require :clasp-analyzer)

;;; Everything
(defparameter *db* (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database #P"lib:compile_commands.json"))

;;; Just the gc_interface.cc file
(defparameter *db*
  (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
   #P"lib:compile_commands.json"
   :selection-pattern "gc_interface.cc"))

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
   :selection-pattern "hashTable.cc"))

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
   :selection-pattern "activationFrame.cc"))

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



(defparameter *db* (ast-tooling:wrapped-jsoncompilation-database-load-from-file (namestring #P"/Users/meister/Development/clasp/build/mpsprep/compile_commands_small.json") :auto-detect))
(defparameter *files* (ast-tooling:get-all-files *db*))
(defparameter *tool* (ast-tooling:new-refactoring-tool *db* *files*))
(ast-tooling:clear-arguments-adjusters *tool*)
(ast-tooling:append-arguments-adjuster *tool* (ast-tooling:get-clang-syntax-only-adjuster))
(ast-tooling:append-arguments-adjuster *tool* (ast-tooling:get-clang-strip-output-adjuster))
(defun concat (&rest vecs)
  (let ((vec (make-vector t 32 t 0))
        (vi 0)
        (curv vecs))
    (tagbody
     top
       (let ((i 0)
             (v (car curv)))
         (setq curv (cdr curv))
         (tagbody
          inner
            (vector-push-extend (elt v i) vec)
            (setq i (+ 1 i))
            (if (< i (length v))
                (go inner)))
         (if curv (go top))))
    vec))
(ast-tooling:append-arguments-adjuster *tool* #'(lambda (args filename)
                                                  (concat args
                                                          (vector "-isystem" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/8.0.0"
                                                                  "-resource-dir" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/8.0.0"))))
(defparameter *asts* (ast-tooling:build-asts *tool*))

(bformat t "About to parse-dynamic-matcher%N")
(defparameter *matcher* (ast-tooling:parse-dynamic-matcher "cxxRecordDecl()"))
(bformat t "About to new-match-finder%N")
(defparameter *finder* (ast-tooling:new-match-finder))
(bformat t "About to with-unmanaged-object%N")
(defclass match-info ()
  ((id-to-node-map :initarg :id-to-node-map :accessor id-to-node-map)
   (ast-context :initarg :ast-context :accessor ast-context)
   (source-manager :initarg :source-manager :accessor source-manager)))
(defclass code-match-callback (ast-tooling:match-callback)
  ((start-of-translation-unit-code :initarg :start-of-translation-unit-code :accessor start-of-translation-unit-code)
   (timer :initarg :timer
          :initform (make-instance 'code-match-timer :name (gensym))
          :accessor timer)
   (match-code :initarg :match-code :accessor match-code)
   (end-of-translation-unit-code :initarg :end-of-translation-unit-code :accessor end-of-translation-unit-code)))
(core:defvirtual ast-tooling:run ((self code-match-callback) match)
  (let* ((nodes (ast-tooling:nodes match))
         (match-info (make-instance 'match-info
                                    :id-to-node-map (ast-tooling:idto-node-map nodes)
                                    :ast-context (match-result-context match)
                                    :source-manager (ast-tooling:source-manager match))))
    (when (match-code self)
      (start-timer (timer self))
      (funcall (match-code self) match-info)
      (stop-timer (timer self))
      (advance-match-counter))))

(defparameter *callback* (make-instance 'code-match-callback :match-code #'lambda (match-info) (bformat t "match-info -> %s%N" match-info)))
(bformat t "About to add-dynamic-matcher%N")
(ast-tooling:add-dynamic-matcher *finder* *matcher* callback)
(defparameter *factory* (ast-tooling:new-frontend-action-factory *finder*))
(bformat t "About to run clang-tool%N")
(ast-tooling:clang-tool-run *tool* *factory*)
