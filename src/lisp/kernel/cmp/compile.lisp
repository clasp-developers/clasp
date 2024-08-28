(in-package #:cmp)

;;;; Top-level interface: CL:COMPILE

(defparameter *lambda-args-num* 0)

(defmacro with-module (( &key module
                           (optimize nil)
                           (optimize-level '*optimization-level*)
                           dry-run) &rest body)
  `(let* ((*the-module* ,module))
     (or *the-module* (error "with-module *the-module* is NIL"))
     (multiple-value-prog1
         (with-irbuilder ((llvm-sys:make-irbuilder (thread-local-llvm-context)))
           ,@body)
       (when (and ,optimize ,optimize-level (null ,dry-run)) (funcall ,optimize ,module ,optimize-level )))))

(defun compile-with-hook (compile-hook definition env)
  (with-compilation-unit ()
    (with-compilation-results ()
      (funcall compile-hook definition env))))

;;; This implements the pure functional part of CL:COMPILE, i.e.
;;; it computes and returns a compiled definition. It also accepts
;;; an environment argument. CL:COMPILE is defined in terms of this.
(defun compile-definition (definition environment)
  (cond
    ((and (typep definition 'core:bytecode-simple-fun)
          (boundp '*btb-compile-hook*)
          *btb-compile-hook*)
     (compile-with-hook *btb-compile-hook* definition environment))
    ((and (typep definition 'core:closure)
          (typep (core:function/entry-point definition)
                 'core:bytecode-simple-fun)
          (boundp '*btb-compile-hook*)
          *btb-compile-hook*)
     (multiple-value-bind (csfun warn fail)
         (compile-with-hook *btb-compile-hook*
                            (core:function/entry-point definition)
                            environment)
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
         #+(or)
         (let ((bc (cmp:bytecompile definition)))
           (if (and (boundp '*btb-compile-hook*) *btb-compile-hook*)
               (funcall *btb-compile-hook* bc nil)
               bc))
         #-(or)
         (funcall cmp:*cleavir-compile-hook* definition environment))))
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

(defun compiler-stats ()
  (core:fmt t "Accumulated finalization time {}%N" llvm-sys:*accumulated-llvm-finalization-time*)
  (core:fmt t "Most recent finalization time {}%N" llvm-sys:*most-recent-llvm-finalization-time*)
  (core:fmt t "Number of compilations {}%N" llvm-sys:*number-of-llvm-finalizations*))

(export 'compiler-stats)
