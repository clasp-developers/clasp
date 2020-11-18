(cl:in-package #:clasp-cleavir)

(defvar *debug-cleavir* nil
  "controls if graphs are generated as forms are being compiled.")
(defvar *debug-cleavir-literals* nil
  "controls if cleavir debugging is carried out on literal compilation. 
when this is t a lot of graphs will be generated.")

(defvar *eliminate-typeq* t
  "Controls whether the typew/typeq elimination phase of the compiler
  runs.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Set the source-position for an instruction
;;;

;;; In CSTs and stuff the origin is (spi . spi). Use the head.
(defun origin-spi (origin)
  (if (consp origin) (car origin) origin))

;;;
;;; the first argument to this function is an instruction that has a
;;; single successor.  whether a go is required at the end of this
;;; function is determined by the code layout algorithm.  
(defgeneric translate-simple-instruction
    (instruction return-value abi current-function-info))

(defparameter *trap-null-origin* nil)
(defmethod translate-simple-instruction :around
    (instruction return-value abi current-function-info)
  (let ((origin (cleavir-ir:origin instruction)))
    (cmp:with-debug-info-source-position ((ensure-origin origin 999902))
      (cmp:with-landing-pad (maybe-entry-landing-pad
                             (cleavir-ir:dynamic-environment instruction)
                             return-value *tags* current-function-info)
        (call-next-method)))))

(defgeneric translate-branch-instruction
    (instruction return-value successors abi current-function-info))

(defmethod translate-branch-instruction :around
    (instruction return-value successors abi current-function-info)
  (let ((origin (cleavir-ir:origin instruction)))
    (cmp:with-debug-info-source-position ((ensure-origin origin 999903))
      (cmp:with-landing-pad (maybe-entry-landing-pad
                             (cleavir-ir:dynamic-environment instruction)
                             return-value *tags* current-function-info)
        (call-next-method)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Helpers for translating HIR locations to LLVM variables.

;;; HIR is not in SSA form, so in general we can use allocas and load/store to
;;; simulate those variables in llvm - it can then run the mem2reg pass to
;;; convert that to SSA and avoid touching memory if possible.
;;; We try to make things easier for LLVM by translating HIR data as SSA
;;; variables if they happen to have only one static definition already.
;;; This ALMOST works, but due to how HIR is generated, in one obscure case
;;; with unwinds, the correct HIR has a use-before-define. See bug #642.
;;; Example: (values (block nil ((lambda () (return (loop))))))
;;; As such, we fall back to an alloca rather than signaling an error.
;;; The better thing to do would be to have Cleavir not generate this kind of
;;; HIR, so we could treat a use-before-define as a bug. FIXME FIXME FIXME

(defvar *datum-variables*)
(defvar *datum-allocas*)

(defun datum-name-as-string (datum)
  ;; We need to write out setf names as well as symbols, in a simple way.
  ;; "simple" means no pretty printer, for a start.
  ;; Using SYMBOL-NAME like this is about 25x faster than using write-to-string,
  ;; and this function is called rather a lot so it's nice to make it faster.
  (let ((name (cleavir-ir:name datum)))
    (if (symbolp name)
        (symbol-name name)
        (write-to-string name
                         :escape nil
                         :readably nil
                         :pretty nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Translate

(defvar *basic-blocks*)
(defvar *ownerships*)
(defvar *tags*)

(defun get-or-create-lambda-name (instr)
  (or (cleavir-ir:name instr) 'TOP-LEVEL))

(defun ensure-origin (origin &optional (num 999905))
  (if origin
      origin
      (core:make-source-pos-info "no-source-info-available" num num num)))

(defun instruction-source-pos-info (instruction)
  "Return a source-pos-info object for the instruction"
  (let ((origin (cleavir-ir:origin instruction)))
    (cond (origin (origin-spi origin))
          (core:*current-source-pos-info*)
          (t (core:make-source-pos-info "no-source-info-available" 0 0 0)))))

(defun log-translate (initial-instruction)
  (let ((mir-pathname (make-pathname :name (sys:bformat nil "mir%d" (incf *debug-log-index*))
                                     :type "gml" :defaults (pathname *debug-log*))))
    (format *debug-log* "About to write mir to ~a~%" (namestring mir-pathname))
    (finish-output *debug-log*)
    (multiple-value-bind (instruction-ids datum-ids)
        (cleavir-ir-gml:draw-flowchart initial-instruction (namestring mir-pathname)))
    (format *debug-log* "Wrote mir to: ~a~%" (namestring mir-pathname)))
  (let ((mir-pathname (make-pathname :name (format nil "mir~a" (incf *debug-log-index*))
                                     :type "dot" :defaults (pathname *debug-log*))))
    (cleavir-ir-graphviz:draw-flowchart initial-instruction (namestring mir-pathname))
    (format *debug-log* "Wrote mir to: ~a~%" (namestring mir-pathname))))

#+debug-monitor
(defun monitor-instructions-with-origins (top-instruction)
  (let ((instr-count (make-hash-table))
        (total 0)
        (have-origins 0))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instr)
       (if (cleavir-ir:origin instr)
           (incf have-origins)
           (incf (gethash (class-of instr) instr-count 0)))
       (incf total))
     top-instruction)
    (sys:monitor-message "instr-origins ~d ~d frac ~f" have-origins total (/ have-origins (float total)))
    (let ((instr-count-list nil))
      (maphash (lambda (class count)
                 (push (cons count class) instr-count-list))
               instr-count)
      (let ((sorted-instr-count-list (sort instr-count-list #'> :key #'car)))
        (loop for num below 5
              for count-class in sorted-instr-count-list
              when count-class
                do (sys:monitor-message "instr-miss-origin-class-count ~a ~a" (car count-class) (cdr count-class)))))))

;;; testy

(defvar *replacement-cache*)
(defvar *assignments-to-delete*)
;;; given a replacement, return a location that all of its uses can
;;; use instead. that may be the location itself if there is no replacement.
(defun replacement (location)
  (or (gethash location *replacement-cache*)
      (setf (gethash location *replacement-cache*)
            (compute-replacement location))))

(defun compute-replacement (location)
  (if (and (typep location 'cleavir-ir:lexical-location) (ssablep location))
      (let ((def (first (cleavir-ir:defining-instructions location))))
        (if (typep def 'cleavir-ir:assignment-instruction)
            ;; okay, it's a temp; now, is it necessary?
            (let ((pre (first (cleavir-ir:inputs def))))
              (if (ssablep pre)
                  ;; no - get a replacement by recursion,
                  ;; and mark the assignment for deletion
                  (progn (push def *assignments-to-delete*)
                         (replacement pre))
                  ;; yes
                  location))
            location))
      location))

;; Used by both CST-to-AST and Generate-AST versions.
(defun log-cst-to-ast (ast)
  (let ((ast-pathname (make-pathname :name (format nil "ast~a" (incf *debug-log-index*))
                                     :type "dot" :defaults (pathname *debug-log*))))
    (cleavir-ast-graphviz:draw-ast ast ast-pathname)
    (core:bformat *error-output* "Just dumped the ast to %s%N" ast-pathname)
    #+(or)(multiple-value-bind (instruction-ids datum-ids)
              (cleavir-ir-gml:draw-flowchart initial-instruction (namestring ast-pathname)))
    (format *debug-log* "Wrote ast to: ~a~%" (namestring ast-pathname))))

(defvar *interactive-debug* nil)

#+cst
(defun conversion-error-handler (condition)
  ;; Resignal the condition to see if anything higher up wants to handle it.
  ;; If not, continue compilation by replacing the errant form with a form
  ;; that will signal an error if it's reached at runtime.
  ;; The nature of this form is a bit tricky because it can't just include
  ;; the original condition, if we're in COMPILE-FILE - conditions aren't
  ;; necessarily dumpable, and nor is the source.
  ;; For now we just assume we're in COMPILE-FILE.
  (signal condition)
  (let* ((cst (cleavir-cst-to-ast:cst condition))
         (form (cst:raw cst))
         (origin (cst:source cst)))
    (invoke-restart 'cleavir-cst-to-ast:substitute-cst
                    (cst:reconstruct
                     `(error 'cmp:compiled-program-error
                             :form ,(with-standard-io-syntax
                                      (write-to-string form
                                                       :escape t :pretty t
                                                       :circle t :array nil))
                             :origin ',(origin-spi origin)
                             :condition ,(princ-to-string condition))
                     cst clasp-cleavir:*clasp-system* :default-source origin))))

#+cst
(defun cst->ast (cst &optional (env *clasp-env*))
  "Compile a cst into an AST and return it.
Does not hoist.
COMPILE might call this with an environment in ENV.
COMPILE-FILE will use the default *clasp-env*."
  (handler-bind
      ((cleavir-env:no-variable-info
         (lambda (condition)
           (cmp:warn-undefined-global-variable
            (origin-spi (cleavir-env:origin condition))
            (cleavir-environment:name condition))
           (invoke-restart 'cleavir-cst-to-ast:consider-special)))
       (cleavir-env:no-function-info
         (lambda (condition)
           (cmp:register-global-function-ref
            (cleavir-environment:name condition)
            (origin-spi (cleavir-env:origin condition)))
           (invoke-restart 'cleavir-cst-to-ast:consider-global)))
       (cleavir-cst-to-ast:compiler-macro-expansion-error
         (lambda (condition)
           (warn 'cmp:compiler-macro-expansion-error-warning
                 :origin (origin-spi (cst:source (cleavir-cst-to-ast:cst condition)))
                 :condition condition)
           (continue condition)))
       ((and cleavir-cst-to-ast:compilation-program-error
             ;; If something goes wrong evaluating an eval-when, we just want a normal error
             ;; signal- we can't recover and keep compiling.
             (not cleavir-cst-to-ast:eval-error))
         #'conversion-error-handler))
    (let ((ast (cleavir-cst-to-ast:cst-to-ast cst env *clasp-system*)))
      (when *interactive-debug* (draw-ast ast))
      (cc-dbg-when *debug-log* (log-cst-to-ast ast))
      (setf *ct-generate-ast* (compiler-timer-elapsed))
      ast)))

#-cst
(defun generate-ast (form &optional (env *clasp-env*))
  "Compile a form into an AST and return it.
Does not hoist."
  (handler-bind
      ((cleavir-env:no-variable-info
         (lambda (condition)
           (cmp:warn-undefined-global-variable
            (origin-spi (cleavir-env:origin condition))(cleavir-environment:name condition))
           (invoke-restart 'cleavir-generate-ast:consider-special)))
       (cleavir-env:no-function-info
         (lambda (condition)
           (cmp:register-global-function-ref (cleavir-environment:name condition))
           (invoke-restart 'cleavir-generate-ast:consider-global))))
    (let ((ast (cleavir-generate-ast:generate-ast form env *clasp-system*)))
      (when *interactive-debug* (draw-ast ast))
      (cc-dbg-when *debug-log* (log-cst-to-ast ast))
      (setf *ct-generate-ast* (compiler-timer-elapsed))
      ast)))

;;; Given an AST that may not be a function-ast, wrap it
;;; in a function AST. Useful for the pattern of
;;; (eval form) = (funcall (compile nil `(lambda () ,form)))
;;; as this essentially does the lambda wrap.
(defun wrap-ast (ast)
  (cleavir-ast:make-function-ast
   ast nil
   :origin (cleavir-ast:origin ast)
   :policy (cleavir-ast:policy ast)))

#+debug-monitor
(defun monitor-ast-with-origins (ast)
  (let ((ast-count (make-hash-table))
        (total 0)
        (have-origins 0))
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (ast)
       (if (cleavir-ast:origin ast)
           (incf have-origins)
           (incf (gethash (class-of ast) ast-count 0)))
       (incf total))
     ast)
    (sys:monitor-message "ast-origins ~d ~d frac ~f" have-origins total (/ have-origins (float total)))
    (let ((ast-count-list nil))
      (maphash (lambda (class count)
                 (push (cons count class) ast-count-list))
               ast-count)
      (let ((sorted-ast-count-list (sort ast-count-list #'> :key #'car)))
        (loop for num below 5
              for count-class in sorted-ast-count-list
              when count-class
                do (sys:monitor-message "ast-miss-origin-class-count ~a ~a" (car count-class) (cdr count-class)))))))

(defparameter *debug-final-gml* nil)
(defparameter *debug-final-next-id* 0)

;;; Clasp cleavir entry point for CL:COMPILE.

;; Set this to T to watch cclasp-compile* run
(defvar *cleavir-compile-verbose* nil)
(export '*cleavir-compile-verbose*)

;;; So that non-cst-client can inherit behaviour
(defclass clasp-eclector-client-mixin ()())

(defclass clasp-cst-client (eclector.concrete-syntax-tree:cst-client clasp-eclector-client-mixin) ())

;; singleton- don't bother with constructor
(defvar *cst-client*
  (locally (declare (notinline make-instance)) (make-instance 'clasp-cst-client)))

(defmethod eclector.parse-result:source-position
    ((client clasp-cst-client) stream)
  (cmp:compile-file-source-pos-info stream))

(defmethod eclector.reader:find-character ((client clasp-eclector-client-mixin) name)
  (or (call-next-method)
      (gethash name *additional-clasp-character-names*)
      (simple-unicode-name name)))

(defun map-make-structure-arguments (initargs)
  (loop for all = initargs then (cddr all)
        for key = (first all) and value = (second all)
        while all
        append (list
                (if (keywordp key) key (intern (string key) :keyword))
                value)))

(defmethod eclector.reader:make-structure-instance ((client clasp-eclector-client-mixin) name initargs)
  ;;; see discussion in https://github.com/s-expressionists/Eclector/issues/63
  ;;; initargs might be string-designators, not keywords, need to transform
  (core::make-structure
   name
   (map-make-structure-arguments initargs)))

(export '(open-debug-log close-debug-log))
  
