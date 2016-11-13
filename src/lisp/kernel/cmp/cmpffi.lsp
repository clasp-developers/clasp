(in-package :cmp)

;;; The *translators* hash-table maps type keywords to llvm types and
;;;   to-object and from-object translators
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *translators* (make-hash-table))
  (defstruct translator name type to-object-name from-object-name))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (gethash :int *translators*)
        (make-translator :name :int
                        :type +i32+
                        :to-object-name "tr_to_object_int"
                        :from-object-name "tr_from_object_int"))
  (defun safe-translator-type (kw)
    (translator-type (gethash kw *translators*)))
  (defun safe-translator-to-object-name (kw)
    (translator-to-object-name (gethash kw *translators*)))
  (defun safe-translator-from-object-name (kw)
    (translator-from-object-name (gethash kw *translators*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Needed by 
  (defun mangled-callback-name (name)
    (format nil "Callback_~a" name)))

;;;
;;; This is what defcallback needs to do to generate a callback function
;;;
(defmacro defcallback (name-and-options return-type-kw arguments &rest body)
  (let ((function-name (if (consp name-and-options)
                           (car name-and-options)
                           name-and-options)))
    `(eval-when (:compile-toplevel :execute)
       ;; Arguments that are passed to the defcallback macro  - needs a BODY
       (let ((function-name ',function-name)
             (body ',body)
             (arguments ',arguments)
             (return-type-kw ,return-type-kw))
         (bformat t "arguments = %s\n" arguments)
         (multiple-value-bind (argument-symbols argument-type-kws)
             (loop for entry in arguments
                collect (first entry) into l1
                collect (second entry) into l2
                finally (return (values l1 l2)))
           (bformat t "argument-symbols: %s\n" argument-symbols)
           (bformat t "argument-type-kws: %s\n" argument-type-kws)
           ;; Convert type keywords into llvm types ie: :int -> +i32+
           (let* ((body-form `(lambda ,argument-symbols (progn ,@body)))
                  (argument-names (mapcar (lambda (s) (string s)) argument-symbols))
                  (mangled-function-name (mangled-callback-name function-name))
                  (return-type (translator-type (gethash return-type-kw *translators*)))
                  (argument-types (mapcar (lambda (type-kw)
                                            (safe-translator-type type-kw))
                                          argument-type-kws))
                  ;; Get the type of the function
                  (function-type (llvm-sys:function-type-get return-type argument-types))
                  ;; Create a new llvm function in the current llvm Module cmp:*the-module*
                  (new-func (irc-function-create function-type
                                                      'llvm-sys:external-linkage
                                                      mangled-function-name
                                                      cmp:*the-module*))
                  (cmp:*current-function* new-func)
                  (cmp:*current-function-name* mangled-function-name)
                  ;; Create an IRBuilder - a helper for adding instructions to func
                  (irbuilder-cur (llvm-sys:make-irbuilder cmp:*llvm-context*)))
             ;; Create the entry basic block in the current function
             (let ((bb (cmp:irc-basic-block-create "entry" cmp:*current-function*)))
               (llvm-sys:set-insert-point-basic-block irbuilder-cur bb)
               ;; Start generating instructions
               (with-irbuilder (irbuilder-cur)
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
                                                  (to-object-func (get-function-or-error *the-module* to-object-name)))
                                             (llvm-sys:create-call-array-ref
                                              cmp:*irbuilder*
                                              to-object-func
                                              (list c-arg)
                                              trans-arg-name nil)))
                                         c-args argument-type-kws argument-names)))
                   ;; (2) Call the closure with the arguments in registers
                   (let* ((real-args (if (< (length cl-args) core:+number-of-fixed-arguments+)
                                         (append cl-args (make-list (- core:+number-of-fixed-arguments+ (length cl-args)) :initial-element (cmp:null-t-ptr)))
                                         cl-args))
                          (function-object (if core:*use-cleavir-compiler*
                                               (funcall (find-symbol "COMPILE-LAMBDA-FORM-TO-LLVM-FUNCTION" :clasp-cleavir) body-form)
                                               (cmp:compile-lambda-function body-form)))
                          (cl-result (llvm-sys:create-call-array-ref
                                      cmp:*irbuilder*
                                      function-object
                                      (list* (cmp:null-t-ptr) (cmp:null-t-ptr) (jit-constant-size_t (length cl-args)) real-args) "cl-result")))
                     ;; (3) Call the translator for the return value
                     (if (eq return-type-kw :void)
                         ;; Return with void
                         (llvm-sys:create-ret-void cmp:*irbuilder*)
                         ;; Return the result
                         (let* ((from-object-name (safe-translator-from-object-name return-type-kw))
                                (from-object-func (get-function-or-error *the-module* from-object-name))
                                (c-result (llvm-sys:create-call-array-ref
                                           cmp:*irbuilder*
                                           from-object-func
                                           (list (llvm-sys:create-extract-value cmp:*irbuilder* cl-result (list 0) "val0"))
                                           "cl-result" nil)))
                           (llvm-sys:create-ret cmp:*irbuilder* c-result)))))))))))))


(defmacro callback (sym)
  `(core:dlsym :rtld-default (mangled-callback-name ',sym)))

(defun get-callback (sym)
  (core:dlsym :rtld-default (mangled-callback-name sym)))
