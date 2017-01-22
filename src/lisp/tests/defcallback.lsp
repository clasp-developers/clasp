(in-package :cmp)


;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; Support functions.
;;;
;;; Delete these and replace them with the more comprehensive ones that frgo wrote

(defun safe-translator-type (type)
  (cond
    ((eq type :int) cmp:+i32+)
    (t (error "Add support for type ~a" type))))

(defun safe-translator-from-object-name (type)
  (cond
    ((eq type :int) "tr_from_object_int")
    (t (error "Add support for safe-translator-from-object-name ~a" type))))

(defun safe-translator-to-object-name (type)
  (cond
    ((eq type :int) "tr_to_object_int")
    (t (error "Add support for safe-translator-to-object-name ~a" type))))


;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; C A L L B A C K  S U P P O R T

;;; This was a collaboration between frgo and drmeister.
;;;   frgo provided the support functions to convert types
;;;   drmeister wrote the do-%defcallback function
;;; CLASP-FFI package.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mangled-callback-name (name)
    (format nil "Callback_~a" name)))

;;; This is what defcallback needs to do to generate a callback function
;;;
;;; TODO: Implement and/or correct the following issues:
;;; 1. Symbols *the-module*, *current-function*,irc-basic-block-create,
;;;    *llvm-context*, *irbuilder*, null-t-ptr, compile-lambda-function, ...
;;; need to be exported properly from cackage cmp
;;;

(defun do-%defcallback (name-and-options return-type-kw argument-symbols argument-type-kws body)
  (multiple-value-bind (function-name convention)
      (if (consp name-and-options)
          (destructuring-bind (tname &key tconvention)
              name-and-options
            (values tname tconvention))
          (values name-and-options :cdecl))
    (format *debug-io* "argument-symbols: ~S~&" argument-symbols)
    (format *debug-io* "argument-type-kws: ~S~&" argument-type-kws)
    ;;(format *debug-io* "argument-names: ~S~&" argument-names)
    ;; Convert type keywords into llvm types ie: :int -> +i32+
    (let* ((body-form `(lambda ,argument-symbols ,@body))
           (argument-names (mapcar (lambda (s) (format *debug-io* "s = ~a~%" s) (string s)) argument-symbols))
           (mangled-function-name (mangled-callback-name function-name))
           (return-type (safe-translator-type return-type-kw ))
           (argument-types (mapcar (lambda (type-kw)
                                     (safe-translator-type type-kw))
                                   argument-type-kws))
           ;; Get the type of the function
           (function-type (llvm-sys:function-type-get return-type argument-types))
           ;; Create a new llvm function in the current llvm Module cmp:*the-module*
           (new-func (llvm-sys:function-create function-type
                                               'llvm-sys:external-linkage
                                               mangled-function-name
                                               cmp::*the-module*))
           (cmp::*current-function* new-func)
           (cmp::*current-function-name* mangled-function-name)
           ;; Create an IRBuilder - a helper for adding instructions to func
           (irbuilder-cur (llvm-sys:make-irbuilder cmp::*llvm-context*)))
      (format t "Created function ~a in ~a~%" new-func cmp:*the-module*)
      ;; Create the entry basic block in the current function
      (let ((bb (cmp::irc-basic-block-create "entry" cmp::*current-function*)))
        (llvm-sys:set-insert-point-basic-block irbuilder-cur bb)
        ;; Start generating instructions
        (cmp::with-irbuilder (irbuilder-cur)
          ;; (1) Call the translators for every argument returning a value in a llvm register
          ;;       Get the c-args from the function argument list
          (let* ((c-args (mapcar #'(lambda (arg argname)
                                     (llvm-sys:set-name arg argname)
                                     arg)
                                 (llvm-sys:get-argument-list new-func) argument-names))
                 ;; Call a translator for each c-arg and accumulate a list of cl-args in registers
                 (cl-args (mapcar (lambda (c-arg arg-type-kw arg-name)
                                    (let* ((to-object-name (safe-translator-to-object-name arg-type-kw))
                                           (trans-arg-name (format nil "translated-~a" arg-name))
                                           ;; Create the function declaration on the fly
                                           (to-object-func (cmp::get-function-or-error cmp::*the-module* to-object-name)))
                                      (llvm-sys:create-call-array-ref
                                       cmp::*irbuilder*
                                       to-object-func
                                       (list c-arg)
                                       trans-arg-name nil)))
                                  c-args argument-type-kws argument-names)))
            ;; (2) Call the closure with the arguments in registers
            (let* ((real-args (if (< (length cl-args) core:+number-of-fixed-arguments+)
                                  (append cl-args (make-list (- core:+number-of-fixed-arguments+ (length cl-args)) :initial-element (cmp::null-t-ptr)))
                                  cl-args))
                   (function-object (if core:*use-cleavir-compiler*
                                        (funcall (find-symbol "COMPILE-LAMBDA-FORM-TO-LLVM-FUNCTION" :clasp-cleavir) body-form)
                                        (cmp::compile-lambda-function body-form)))
                   (cl-result (llvm-sys:create-call-array-ref
                               cmp::*irbuilder*
                               function-object
                               (list* (cmp::null-t-ptr) (cmp::null-t-ptr) (jit-constant-size_t (length cl-args)) real-args) "cl-result")))
              ;; (3) Call the translator for the return value
              (if (eq return-type-kw :void)
                  ;; Return with void
                  (llvm-sys:create-ret-void cmp::*irbuilder*)
                  ;; Return the result
                  (let* ((from-object-name (safe-translator-from-object-name return-type-kw))
                         (from-object-func (cmp::get-function-or-error cmp::*the-module* from-object-name))
                         (c-result (llvm-sys:create-call-array-ref
                                    cmp::*irbuilder*
                                    from-object-func
                                    (list (llvm-sys:create-extract-value cmp::*irbuilder* cl-result (list 0) "val0"))
                                    "cl-result" nil)))
                    (llvm-sys:create-ret cmp::*irbuilder* c-result))))))))))

(defmacro %defcallback (name-and-options return-type-kw argument-symbols argument-type-kws &rest body)
  (format t "In %defcallback macro~%")
  (do-%defcallback name-and-options return-type-kw argument-symbols argument-type-kws body)
  nil)

(defmacro %callback (sym)
  `(%dlsym (mangled-callback-name ',sym)))

(defun %get-callback (sym)
  (%dlsym (mangled-callback-name sym)))


(export '%defcallback)


;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;;
;;; Test it with something like this...
;;;
;;; In test-defcallback.lisp...
;;;      (cmp:%defcallback first :int (x y) (:int :int) (+ x y))
;;;
;;; Then...
;;;      (compile-file "test-defcallback.lisp")
;;;

