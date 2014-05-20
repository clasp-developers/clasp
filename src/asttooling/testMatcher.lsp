(use-package :ast-tooling)
(use-package :clang-ast)

(require 'clang-tool)


(defparameter *simple-matcher*
  (assemble-matcher '(:bind :field (:field-decl (:has-type (:record-decl (:has-name "TinyStruct")))))))

(format t "*simple-matcher* = ~a~%" *simple-matcher*)


(defparameter *match-counter* 0)

(defclass field-match (match-callback) () )
(core:defvirtual run ((self field-match) match)
  (format t "----------------  hit field-match run~%")
  (let* ((nodes (nodes match))
         (id-to-node-map (idto-node-map nodes))
         (node (gethash :field id-to-node-map))
         (context (context match))
         (source-manager (source-manager match))
         (lang-options (get-lang-opts context))
         (begin (get-loc-start node))
         (_end (get-loc-end node))
         (end (lexer-get-loc-for-end-of-token _end 0 source-manager lang-options))
         (token-range (new-char-source-range-get-char-range begin end)))
    (multiple-value-bind (source invalid)
        (lexer-get-source-text token-range source-manager lang-options)
      (format t "Got a match with node: ~a   name: ~a~%" node (cast:get-qualified-name-as-string node))
      (format t "begin location: ~a~%" (print-to-string begin source-manager))
      (format t "end location: ~a~%" (print-to-string end source-manager))
      (format t "Did lexer-get-source-text return valid source --> ~:[no~;yes~]~%" (not invalid))
      (format t "------------source from Lexer::getSourceText(CharSourceRange(begin,end),...) starts below~%")
      (format t "~a~%" source)
      (format t "------------source ends~%")
      (cast:dump node)
      (setq *match-counter* (1+ *match-counter*))
      )))

(defclass dump-match-callback (match-callback) () )
(core:defvirtual run ((self dump-match-callback) match)
  (let* ((nodes (nodes match))
         (id-to-node-map (idto-node-map nodes))
         (node (gethash :whole id-to-node-map)))
    (cast:dump node)
    ))



(defclass count-match-callback (match-callback) () )
(core:defvirtual run ((self count-match-callback) match)
  (let* ((nodes (nodes match))
         (id-to-node-map (idto-node-map nodes))
         (node (gethash :whole id-to-node-map)))
    (setq *match-counter* (1+ *match-counter*))
    ))


(defparameter *match-id-to-node-map* nil)
(defparameter *match-ast-context* nil)
(defparameter *match-source-manager* nil)
;;Requires a lambda CODE that takes no arguments:
;;but runs in a dynamic environment where *match-id-to-node-map*
;;*match-ast-context* and *match-source-manager* are defined for the match
(defclass code-match-callback (match-callback)
  ((code :initarg :code :accessor callback-code)
   )
  )
(core:defvirtual run ((self code-match-callback) match)
  (let* ((nodes (nodes match))
         (*match-id-to-node-map* (idto-node-map nodes))
         (*match-ast-context* (context match))
         (*match-source-manager* (source-manager match)))
    (prog1
        (funcall (callback-code self))
      (setq *match-counter* (1+ *match-counter*))
    )))



(defun mtag-source (tag)
  "Get the source code for the current tag
This can only be run in the context set up by the code-match-callback::run method"
  (let* ((node (gethash tag *match-id-to-node-map*))
         (lang-options (get-lang-opts *match-ast-context*))
         (begin (get-loc-start node))
         (_end (get-loc-end node))
         (end (lexer-get-loc-for-end-of-token _end 0 *match-source-manager* lang-options))
         (token-range (new-char-source-range-get-char-range begin end)))
    (multiple-value-bind (source invalid)
        (lexer-get-source-text token-range *match-source-manager* lang-options)
      source)))


(defun mtag-source-start (tag)
  "Get the source code for the current tag
This can only be run in the context set up by the code-match-callback::run method"
  (let* ((node (gethash tag *match-id-to-node-map*))
         (begin (get-loc-start node)))
    (print-to-string begin *match-source-manager*)))

(defparameter *matcher-callback* (make-instance 'field-match))

(defparameter *match-finder* (new-match-finder))
(add-dynamic-matcher *match-finder* *simple-matcher* *matcher-callback*)


(defparameter *match-replacements* nil)
(defmacro mtag-replace (tag fmt &rest fmt-args)
  (let ((rep-src-gs (gensym))
        (node-gs (gensym))
        (begin-gs (gensym))
        (end-gs (gensym))
        (rep-range-gs (gensym))
        (rep-gs (gensym))
        )
  `(let* ((,rep-src-gs (format nil ,fmt ,@fmt-args))
          (,node-gs (gethash ,tag *match-id-to-node-map*))
          (,begin-gs (get-loc-start ,node-gs))
          (,end-gs (get-loc-end ,node-gs))
          (,rep-range-gs (new-char-source-range-get-token-range ,begin-gs ,end-gs))
          (,rep-gs (new-replacement *match-source-manager* ,rep-range-gs ,rep-src-gs)))
     (push ,rep-gs *match-replacements*))))


(defvar *asts* nil)
(defparameter *db* (ast-tooling:jsoncompilation-database-load-from-file "../main/compile_commands.json"))
(defparameter $* (map 'list #'identity (ast-tooling:get-all-files *db*)))
(defparameter $test (subseq $* 0 3))   ; first 3 files in $test

(defmacro lnew (list-name) "Create a dynamic variable to contain a list of names" `(defvar ,list-name nil))
(defun lsel (list-name regex-str)
  "Select file names from the list and return them"
  (let ((re (core:make-regex regex-str)))
    (remove-if-not #'(lambda (x) (core:regex-matches re x)) list-name)))
(defun lsubseq (list-name begin end)
  "Select file names from the list by index from BEGIN to END"
  (subseq list-name begin end))

(defmacro lsize (list-name)
  `(format t "List size ~a~%" (length ,list-name)))

(defmacro ladd (list-name names)
  `(progn
     (defparameter ,list-name (remove-duplicates (append ,list-name ,names) :test #'string=))
     (lsize ,list-name)))

(defmacro lclear (list-name)
  `(defparameter ,list-name nil))

(defmacro llist (list-name)
  `(progn
     (mapc #'(lambda (x) (format t "~a~%" x)) ,list-name)
     (lsize ,list-name)))


(defun lgather-asts (list-name)
  (let* ((files list-name)
         (tool (ast-tooling:new-refactoring-tool *db* files))
         (syntax-only-adjuster (ast-tooling:make-clang-syntax-only-adjuster))
         (strip-output-adjuster (ast-tooling:make-clang-strip-output-adjuster)))
    ;;         (factory (new-frontend-action-factory match-finder)))
    (ast-tooling:clear-arguments-adjusters tool)
    (ast-tooling:append-arguments-adjuster tool syntax-only-adjuster)
    (ast-tooling:append-arguments-adjuster tool strip-output-adjuster)
    ;;    (ast-tooling:run tool factory)
    (format t "Loading ASTs for the files: ~a~%" files)
    (time 
     (multiple-value-bind (num asts)
         (ast-tooling:build-asts tool #())
       (setq *asts* asts)
       (format t "Built asts: ~a~%" asts)
       )
     )))


(defparameter *matches* nil)
(defparameter *test-ast* nil)
(defparameter *test-matcher* nil)
(defparameter *test-match-finder* nil)

(defmacro run-matcher (match-sexp &key callback)
  `(let ((*match-counter* 0))
     (setq *test-ast* '(:bind :whole ,match-sexp))
     (setq *test-matcher* (assemble-matcher *test-ast*))
     (setq *test-match-finder* (new-match-finder))
     (add-dynamic-matcher *test-match-finder* *test-matcher* ,callback)
     (map 'list #'(lambda (x) (match-ast *test-match-finder* (get-astcontext x))) *asts*)
     (format t "Number of matches ~a~%" *match-counter*)
     )
  )


(defmacro match-count (match-sexp)
  `(time (run-matcher ,match-sexp :callback (make-instance 'count-match-callback))))

(defmacro match-dump (match-sexp)
  `(time (run-matcher ,match-sexp :callback (make-instance 'dump-match-callback))))

(defmacro match-run (match-sexp code)
  "Run code on every match"
  `(progn
     (setq *match-replacements* nil) ;; reset *match-replacements*
     (time (run-matcher ,match-sexp :callback (make-instance 'code-match-callback :code ,code)))))



(defun dump-replacements (begin end)
  (do ((idx begin (1+ idx)))
      ((>= idx end) nil)
    (format t "#~a: ~a~%" idx (to-string (elt *match-replacements* idx)))))
       


(format t "Use (gather-asts [:test t]) - to gather the ASTs into *asts*~%")

(format t "Run the *match-finder* on an AST with:~%(match-ast *match-finder* (get-astcontext (elt *asts* 0)))~%")




#|

(match-run (:goto-stmt) #'(lambda () (format t "~a~%" (mtag-source :whole))))
(match-run (:if-stmt (:has-condition (:bind :cond (:expr)))) #'(lambda () (format t "~a~%" (mtag-source-start :cond))))
|#
