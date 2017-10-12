(in-package #:clasp-sandbox)

;;;; Fill an environment with variables and operators and such.

(defmacro macro-lambda (name lambda-list &body body)
  (sys::expand-defmacro name lambda-list body))

(defmacro compiler-macro-lambda (name lambda-list &body body)
  (sys::expand-defmacro name lambda-list body 'define-compiler-macro))

(defun install-sandbox-accessors (environment)
  (macrolet ((def (name lambda-list &body body)
               `(setf (sicl-genv:fdefinition ',name environment)
                      (lambda ,lambda-list
                        (declare (core:lambda-name ,name))
                        ,@body))))
    (def cl:fboundp (function-name) (sicl-genv:fboundp function-name environment))
    (def cl:fdefinition (function-name) (sicl-genv:fdefinition function-name environment))
    (def (setf cl:fdefinition) (new-function function-name)
      (setf (sicl-genv:fdefinition function-name) new-function))
    (def cl:fmakunbound (function-name) (sicl-genv:fmakunbound function-name environment))

    (def cl:special-operator-p (symbol)
      (sicl-genv:special-operator symbol environment))

    ;; We can't just have &optional (env nil) as NIL is a designator for the global environment.
    (def cl:macro-function (symbol &optional env)
      (let ((env (if (null env) environment env)))
        (sicl-genv:macro-function symbol env)))
    (def (setf cl:macro-function) (new-function symbol &optional env)
      (let ((env (if (null env) environment env)))
        (setf (sicl-genv:macro-function symbol env) new-function)))

    ;;; TODO: boundp
    (def cl:symbol-value (symbol)
      ;; this is inefficient in that it will look up the cell (a hash lookup)
      ;; every time. but since this is just the function it might be okay.
      (core:symbol-value-from-cell
       symbol (sicl-genv:variable-cell symbol environment)
       (sicl-genv:variable-unbound symbol environment)))
    (def (setf cl:symbol-value) (value symbol)
      (core:setf-symbol-value-from-cell
       symbol value (sicl-genv:variable-cell symbol environment)))
    ;;; TODO: makunbound

    ;;; only really a closure for the optional default.
    (def cl:find-class (symbol &optional (errorp t) (env environment))
      (let ((class (sicl-genv:find-class symbol env)))
        (or class (error "Could not find class ~a" symbol))))
    (def (setf cl:find-class) (new-class symbol &optional errorp (env environment))
      (setf (sicl-genv:find-class symbol env) new-class))

    ;;; TODO: proclaim, constantp?
    ;;; TODO: packages?
    ))

(defun install-basics (environment)
  (setf (sicl-genv:constant-variable 'sicl-genv:+global-environment+ environment) environment)
  (setf (sicl-genv:fdefinition 'sicl-genv:global-environment environment)
        (lambda () environment))
  (macrolet ((copy (name)
               `(setf (sicl-genv:fdefinition ',name environment) #',name)))
    (copy sicl-genv:macro-function)
    (copy sicl-genv:function-cell)
    (copy sicl-genv:variable-cell)
    (copy sicl-genv:variable-unbound)
    (copy core:symbol-value-from-cell)
    (copy core:setf-symbol-value-from-cell)
    (copy core:multiple-value-funcall)
    ;; used in defmacro/destructuring-bind
    (copy core::dm-too-few-arguments) (copy core::dm-too-many-arguments)
    ;; used from backquote
    (copy core:backquote-append)
    (copy core:backquote-append-list))
  (setf (sicl-genv:macro-function 'core:backquote environment) (macro-function 'core:backquote)))

(defun install-default-setf-expander (environment)
  ;; basically copied from SICL/Code/Compiler/Extrinsic-environment/define-default-setf-expander.lisp
  ;; not that there are many ways to do it.
  (setf (sicl-genv:default-setf-expander environment)
        (lambda (form env)
          (declare (ignore env))
          (let ((store (gensym "STORE")))
            (if (symbolp form)
                (values nil nil (list store) `(setq ,form ,store) form)
                (let ((temps (loop for arg in (rest form) collect (gensym))))
                  (values temps (rest form) (list store)
                          `(funcall #'(setf ,(first form)) ,store ,@temps)
                          `(,(first form) ,@temps))))))))

(defun install-cl-special-operators (environment)
  (dolist (s '(block let* return-from
               #|catch|# load-time-value setq
               eval-when locally symbol-macrolet
               flet macrolet tagbody
               function #|multiple-value-call|# the
               go multiple-value-prog1 #|throw|#
               if progn #|unwind-protect|#
               labels #|progv|# let quote))
    ;; no m-v-c since it takes a function designator
    ;; the other commented aren't provided by cleavir
    (setf (sicl-genv:special-operator s environment) t)))

(defun install-truly-the (environment)
  ;; used in defmacro expansion, among other places
  (setf (sicl-genv:macro-function 'ext:truly-the environment)
        (macro-lambda ext:truly-the (type form)
          `(the ,type ,form))))

(defun install-cleavir-primops (environment)
  (do-external-symbols (s "CLEAVIR-PRIMOP")
    (if (eq s 'cleavir-primop:call-with-variable-bound)
        (setf (sicl-genv:fdefinition s environment) (fdefinition s))
        (setf (sicl-genv:special-operator s environment) t))))

;;; CL functions that cannot be used without opening the veil.
(defparameter *cl-environment*
  '(;; "core" functions that need to be defined with sicl-genv
    proclaim fdefinition symbol-function symbol-value find-class get-setf-expansion
    fboundp fmakunbound boundp makunbound special-operator-p macro-function
    constantp ; "core" behavior is distinguishing constant variable
    ;; package designators and generally packages (some also core)
    export find-symbol find-package find-all-symbols import list-all-packages rename-package
    shadow shadowing-import delete-package make-package unexport unintern unuse-package use-package
    intern package-name package-nicknames package-shadowing-symbols package-use-list package-used-by-list
    ;; have a defaulting optional environment parameter
    make-load-form make-load-form-saving-slots
    macroexpand-1 macroexpand
    ;; class/condition type designators
    make-instance error cerror signal warn make-condition
    ;; function designators
    funcall apply map map-into mapc mapcar mapcan mapl maplist mapcon
    subst subst-if subst-if-not nsubst nsubst-if nsubst-if-not
    member member-if member-if-not
    assoc assoc-if assoc-if-not rassoc rassoc-if rassoc-if-not
    intersection nintersection adjoin
    set-exclusive-or nset-exclusive-or subsetp union nunion
    every some notevery notany reduce count count-if count-if-not
    sort stable-sort find find-if find-if-not position position-if position-if-not
    search mismatch substitute substitute-if substitute-if-not
    nsubstitute nsubstitute-if nsubstitute-if-not merge
    remove remove-if remove-if-not delete delete-if delete-if-not
    remove-duplicates delete-duplicates
    make-hash-table ; it does take a designator, though severely restricted
    maphash set-pprint-dispatch set-dispatch-macro-character set-macro-character
    disassemble ed documentation
    ;; types
    subtypep typep
    upgraded-complex-part-type upgraded-array-element-type
    make-array adjust-array make-sequence coerce concatenate
    open
    ;; some functions also return type specifiers, function designators, etc, which could be hairy.
    ;; ~//
    format
    ;; Reader macro functions can be designators
    read read-preserving-whitespace read-from-string
    ;; obvious
    eval load compile compile-file))

;;; ...that touch the filesystem
(defvar *cl-filesystem*
  '(open with-open-file
    directory probe-file ensure-directories-exist truename file-author file-write-date rename-file
    delete-file))

(defun import-cl-functions (environment exceptions)
  (do-external-symbols (s "COMMON-LISP")
    (unless (member s exceptions)
      (when (and (fboundp s)
                 (not (macro-function s))
                 (not (special-operator-p s)))
        (setf (sicl-genv:fdefinition s environment) (fdefinition s)))
      (let ((s `(setf ,s)))
        (when (fboundp s)
          (setf (sicl-genv:fdefinition s environment) (fdefinition s)))))))

(defun install-defmacro (environment)
  (setf (sicl-genv:macro-function 'defmacro environment)
        ;; This doesn't actually close over ENVIRONMENT.
        (macro-lambda defmacro (name lambda-list &body body)
          (multiple-value-bind (function pprint docstring)
              (sys::expand-defmacro name lambda-list body)
            `(eval-when (:compile-toplevel :load-toplevel :execute)
               (setf (macro-function ',name)
                     #',function)
               ',name)))))

;;; which macros are "safe" depends on their (implementation-defined) expansions.
;;; These are quite possibly "safe" in general though.
;;; but expansions may require implementation functions to run!
(defparameter *safe-macros*
  '(nth-value prog prog2 psetq case with-hash-table-iterator dotimes prog* loop with-open-stream
    with-standard-io-syntax with-accessors when prog1 call-method do do* and with-compilation-unit
    print-unreadable-object multiple-value-setq unless destructuring-bind with-slots cond
    multiple-value-bind dolist multiple-value-list loop-finish lambda time return with-output-to-string
    or ecase
    ;; in clasp these are macros, and safe ones, just converting to calls.
    unwind-protect throw catch))

(defparameter *setf-macros*
  '(setf psetf shiftf rotatef
    push pop remf pushnew
    incf decf))

;;; Macros that clasp implements special operators with.
;;; Multiple-value-call is excluded because function designator resolution is environment-dependent.
(defparameter *special-macros*
  '(unwind-protect progv catch throw))

(defun import-macros (environment macros)
  (dolist (s macros)
    (setf (sicl-genv:macro-function s environment)
          (macro-function s))))

;;; I want this one earlier because we need it for multiple-value-call, funcall, etc basic things
(defun install-coerce-fdesignator (environment)
  (setf (sicl-genv:fdefinition 'coerce:fdesignator environment)
        (lambda (fdesignator)
          (etypecase fdesignator
            (symbol (sicl-genv:fdefinition fdesignator environment))
            (function fdesignator)))))

(defun install-multiple-value-call (environment)
  (setf (sicl-genv:macro-function 'cl:multiple-value-call environment)
        (macro-lambda multiple-value-call (function-form &rest forms)
          #+clasp
          (if (eql (length forms) 1)
              `(cleavir-primop:multiple-value-call (coerce:fdesignator ,function-form) ,@forms)
              `(core:multiple-value-funcall
                (coerce:fdesignator ,function-form)
                ,@(mapcar (lambda (x) `#'(lambda () (progn ,x))) forms)))
          #-clasp
          `(cleavir-primop:multiple-value-call
               (coerce:fdesignator ,function-form)
             ,@forms))))

(defun install-funcall (environment)
  (setf (sicl-genv:fdefinition 'funcall environment)
        ;; FIXME: we could use a more basic apply that doesn't check designators or the type.
        ;; Also this is an awkward way to write.
        ;; Note the cl: are unnecessary, just intended to emphasize this is in no way recursive.
        (let ((coerce (sicl-genv:fdefinition 'coerce:fdesignator environment)))
          (lambda (fdesignator &rest arguments)
            (cl:apply (cl:funcall coerce fdesignator) arguments))))
  (setf (sicl-genv:compiler-macro-function 'funcall environment)
        (compiler-macro-lambda funcall (fdesignator &rest arguments)
          `(cleavir-primop:funcall (coerce:fdesignator ,fdesignator) ,@arguments))))

;;; Intended as a one-shot function to get the most I've got working.
;;; It's not all in one function so that environments can be set up differently or more finely in the future.
(defun fill-environment (environment)
  (install-basics environment)
  (install-sandbox-accessors environment)
  (install-cl-special-operators environment)
  (install-truly-the environment)
  (install-cleavir-primops environment)
;  (import-macros environment *special-macros*) ; not all have macro definitions in clasp at the moment.
  (import-macros environment *safe-macros*)
  (import-macros environment *setf-macros*)
  (install-default-setf-expander environment)
  (install-coerce-fdesignator environment)
  (install-funcall environment)
  (install-multiple-value-call environment)
  (import-cl-functions environment *cl-environment*)
  (values))
