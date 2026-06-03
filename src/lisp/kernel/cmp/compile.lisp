(in-package #:cmp)

;;;; Top-level interface: CL:COMPILE

(defun coerce-to-lexenv (thing)
  (typecase thing
    (null (make-null-lexical-environment))
    (lexenv thing)
    (t ; assume cleavir. FIXME
     (funcall (find-symbol "CLEAVIR-ENV->BYTECODE" "CLASP-CLEAVIR") thing))))

;; early sham definition. Actual definition in cleavir/compile-bytecode
(declaim (notinline btb-compile))
(defun btb-compile (definition environment)
  (declare (ignore environment))
  definition)

;;; This implements the pure functional part of CL:COMPILE, i.e.
;;; it computes and returns a compiled definition. It also accepts
;;; an environment argument. CL:COMPILE is defined in terms of this.
(defun compile-definition (definition environment)
  (cond
    ((and (typep definition 'core:bytecode-simple-fun) *compile-native*)
     (with-compilation-unit ()
       (with-compilation-results ()
         (btb-compile definition environment))))
    ((and (typep definition 'core:closure)
          (typep (core:function/entry-point definition)
                 'core:bytecode-simple-fun)
          *compile-native*)
     (multiple-value-bind (csfun warn fail)
         (with-compilation-unit ()
           (with-compilation-results ()
             (btb-compile (core:function/entry-point definition) environment)))
       (let ((cells nil))
         (dotimes (i (core:closure-length definition))
           (push (core:closure-ref definition i) cells))
         (values (apply #'core:make-closure csfun (nreverse cells))
                 warn fail))))
    ((typep definition 'generic-function)
     ;; TODO? Compile discriminator
     (clos::compile-generic-function-methods definition))
    ((compiled-function-p definition) (values definition nil nil))
    ((functionp definition)
     (error "COMPILE doesn't know how to handle ~a definition"))
    ((and (consp definition) (eq (car definition) 'lambda))
     (with-compilation-unit ()
       (with-compilation-results ()
         (let ((bc (cmp:bytecompile definition environment)))
           (if *compile-native*
               (btb-compile bc environment)
               bc)))))
    (t (error "COMPILE doesn't know how to handle ~a" definition))))

(defun compile (name &optional definition)
  (multiple-value-bind (function warnp failp)
      ;; Get the actual compiled function and warnp+failp.
      (compile-definition (if (null definition)
                              (if (fboundp name)
                                  (fdefinition name)
                                  (error "No definition for ~a" name))
                              definition)
                          nil)
    ;; Bind the name if applicable.
    (cond ((and (symbolp name) (macro-function name))
           (setf (macro-function name) function)
           (values name warnp failp))
          (name
           (setf (fdefinition name) function)
           (values name warnp failp))
          (t (values function warnp failp)))))

(defun ext:compile-source (lambda-expression &optional source environment)
  "Compile LAMBDA-EXPRESSION in ENVIRONMENT. If SOURCE is non-null it must be a source object returned from EXT:READ-SOURCE or EXT:AUGMENT-SOURCE, and is used to provide source locations for compiled code and error messages."
  (cond (source
         (check-type source hash-table)
         (let ((*source-locations* source))
           (compile-definition lambda-expression environment)))
        (t (compile-definition lambda-expression environment))))
