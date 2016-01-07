(print "Testing")
(require :asdf)
(require :clang-tool)
(require :clasp-analyzer)
(print "Done")

(in-package :clasp-analyzer)

(defparameter *db*
  (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
   #P"app-resources:build-databases;clasp_compile_commands.json"
   :selection-pattern ".*cons\.cc.*$" ))

(defparameter *project* (search/generate-code *db*))



(defparameter *db*
  (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
   #P"app-resources:build-databases;clasp_compile_commands.json"))

(time (defparameter *project* (search/generate-code *db*)))



(macroexpand '(setf (analysis-inline analysis) 'foo))


(ext:chdir #P"/Users/meister/Development/clasp/src/main/")
(clasp-analyzer:load-compilation-database "app-resources:build-databases;clasp_compile_commands.json")
(defun translate-include (args)
  (dotimes (i (length args))
    (when (string= (elt args i) "-Iinclude")
      (setf (elt args i) "-I/Users/meister/Development/clasp/src/main/include")))
  args)
(time (clasp-analyzer:serial-search-all-then-generate-code-and-quit :arguments-adjuster #'translate-include :test t))

(defparameter *test-search* (clasp-analyzer:lsel $* ".*primitives.*"))
(serial-search-only :test *test-search* :arguments-adjuster #'translate-include)
(room)




(apropos "version")

(asdf:asdf-version)

(room)




(load-project)
(analyze-project)
(setf (analysis-inline *analysis*) '("core::Cons_O"))
(generate-code)

(print "Go go go")

(clasp-analyzer:load-compilation-database "app-resources:build-databases;clasp_compile_commands.json")

(clasp-analyzer:lnew $tiny-test-search)

(setq $tiny-test-search (clasp-analyzer:lsel clasp-analyzer:$* ".*/cons\.cc"))

(clasp-analyzer:load-asts $tiny-test-search
           :arguments-adjuster (clasp-analyzer:build-arguments-adjuster))

(defparameter *test-matcher*
  '(:record-decl
    ;;        (:is-definition)
    ;;        (:is-template-instantiation)
    (:matches-name ".*GCInfo.*"))
  )
(clasp-analyzer:match-run
 *test-matcher*
 :limit 10
 ;;              :tag :point
;; :match-comments '( ".*mytest.*" )
 :the-code-match-callback
 #'(lambda ()
     (let* ((decl (mtag-node :whole))
            (args (cast:get-template-args decl))
            (arg (cast:template-argument-list-get args 0))
            (qtarg (cast:get-as-type arg))
            (tsty-new (cast:get-type-ptr-or-null qtarg))
            (key (record-key tsty-new))
            (classified (classify-ctype tsty-new)))
       (format t "MATCH: ------------------~%")
       (format t "        Start: ~a~%" (mtag-loc-start :whole))
       (format t "         Node: ~a~%" (mtag-node :whole))
       (format t "     type-of node: ~a~%" (type-of (mtag-node :whole)))
       (format t "         Name: ~a~%" (mtag-name :whole))
       (format t "          Arg: ~a~%" classified)
       (format t "          key: ~a~%" key)
       )))



(room)
