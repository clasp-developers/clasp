(in-package #:sandbox-environment)

;;;; Fill an environment with variables and operators and such.

(defmacro macro-lambda (name lambda-list &body body)
  #+sbcl
  (sb-cltl2:parse-macro name lambda-list body)
  #+clasp
  (sys::expand-defmacro name lambda-list body)
  #-(or sbcl clasp)
  (error "macro-lambda unimplemented"))

(defmacro compiler-macro-lambda (name lambda-list &body body)
  #+sbcl
  ;; bad
  (sb-int:make-macro-lambda (sb-c::debug-name 'compiler-macro) lambda-list body
                            'define-compiler-macro name)
  #+clasp
  (sys::expand-defmacro name lambda-list body 'define-compiler-macro)
  #-(or sbcl clasp)
  (error "compiler-macro-lambda unimplemented"))

;;; FIXME move
#-clasp
(defun symbol-value-from-cell (name cell unbound)
  (let ((value (car cell)))
    (if (eq value unbound)
        (error 'unbound-variable :name name)
        value)))

(defun install-sandbox-accessors (environment)
  (macrolet ((def (name lambda-list &body body)
               `(setf (sicl-genv:fdefinition ',name environment)
                      (lambda ,lambda-list
                        (declare (core:lambda-name ,name))
                        ,@body))))
    (def cl:fboundp (function-name) (sicl-genv:fboundp function-name environment))
    (def cl:fdefinition (function-name) (sicl-genv:fdefinition function-name environment))
    (def (setf cl:fdefinition) (new-function function-name)
      (setf (sicl-genv:fdefinition function-name environment) new-function))
    (def cl:fmakunbound (function-name) (sicl-genv:fmakunbound function-name environment))

    (def cl:special-operator-p (symbol)
      (if (symbolp symbol)
          (sicl-genv:special-operator symbol environment)
          nil))

    ;; We can't just have &optional (env nil) as NIL is a designator for the global environment.
    (def cl:macro-function (symbol &optional env)
      (let ((env (if (null env) environment env)))
        (sicl-genv:macro-function symbol env)))
    (def (setf cl:macro-function) (new-function symbol &optional env)
      (let ((env (if (null env) environment env)))
        (setf (sicl-genv:macro-function symbol env) new-function)))

    (def cl:compiler-macro-function (function-name &optional env)
      (let ((env (if (null env) environment env)))
        (sicl-genv:compiler-macro-function function-name env)))
    (def (setf cl:compiler-macro-function) (cmf function-name &optional env)
      (let ((env (if (null env) environment env)))
        (setf (sicl-genv:compiler-macro-function function-name env) cmf)))

    ;;; TODO: boundp
    (def cl:boundp (symbol)
      #-clasp
      (not (eq (car (sicl-genv:variable-cell symbol environment))
               (sicl-genv:variable-unbound symbol environment)))
      #+clasp
      (error "boundp unimplemented. Symbol: ~a" symbol))
    (def cl:symbol-value (symbol)
      ;; this is inefficient in that it will look up the cell (a hash lookup)
      ;; every time. but since this is just the function it might be okay.
      ;; a compiler-macro could get the cell as a load time value, for constant symbol.
      ;; (also the unbound)
      #-clasp
      (symbol-value-from-cell
       symbol (sicl-genv:variable-cell symbol environment)
       (sicl-genv:variable-unbound symbol environment))
      #+clasp
      (core:symbol-value-from-cell
       symbol (sicl-genv:variable-cell symbol environment)
       (sicl-genv:variable-unbound symbol environment)))
    (def (setf cl:symbol-value) (value symbol)
      #-clasp
      (setf (car (sicl-genv:variable-cell symbol environment)) value)
      #+clasp
      (core:setf-symbol-value-from-cell
       symbol value (sicl-genv:variable-cell symbol environment)))
    (def cl:makunbound (symbol)
      #-clasp
      (setf (car (sicl-genv:variable-cell symbol environment))
            (sicl-genv:variable-unbound symbol environment))
      #+clasp
      (error "makunbound unimplemented. Symbol: ~a" symbol))
    ;; sort of an accessor.
    (def cleavir-primop:call-with-variable-bound (variable value thunk)
      ;; fixme: atomicity
      (let* ((cell (sicl-genv:variable-cell variable env))
             (old (car cell)))
        (setf (car cell) value)
        (unwind-protect (funcall thunk)
          (setf (car cell) old))))

    ;;; only really a closure for the optional default.
    (def cl:find-class (symbol &optional (errorp t) env)
      (when (null env) (setf env environment))
      (let ((class (sicl-genv:find-class symbol env)))
        (cond (class class)
              (errorp (error "Could not find class ~a" symbol))
              (t nil))))
    (def (setf cl:find-class) (new-class symbol &optional errorp (env environment))
      (setf (sicl-genv:find-class symbol env) new-class))

    #+(or)
    (def ext:symbol-macro (symbol &optional env)
      (let ((env (if (null env) environment env)))
        (sicl-genv:symbol-macro symbol env)))
    #+(or)
    (def (setf ext:symbol-macro) (expansion symbol &optional env)
      (let ((env (if (null env) environment env)))
        (setf (sicl-genv:symbol-macro symbol env) expansion)))

    (def cl:proclaim (declaration-specifier)
      (check-type declaration-specifier cons "a declaration specifier")
      (case (car declaration-specifier)
        ((declaration)
         (destructuring-bind (name) (cdr declaration-specifier)
           (setf (sicl-genv:declaration name environment) t)))
        ((ftype)
         (destructuring-bind (type &rest function-names)
             (cdr declaration-specifier)
           (loop for name in function-names
                 do (setf (sicl-genv:function-type name environment) type))))
        ((inline)
         (loop for name in (cdr declaration-specifier)
               do (setf (sicl-genv:function-inline name environment) 'inline)))
        ((notinline)
         (loop for name in (cdr declaration-specifier)
               do (setf (sicl-genv:function-inline name environment) 'notinline)))
        ((optimize)
         (setf (sicl-genv:optimize-quality-values environment)
               (cleavir-compilation-policy:normalize-optimize
                (cdr declaration-specifier) environment)))
        ((special)
         (loop for var in (cdr declaration-specifier)
               do (setf (sicl-genv:special-variable var environment nil) nil)))
        ((type)
         (destructuring-bind (type &rest variable-names)
             (cdr declaration-specifier)
           (loop for name in variable-names
                 do (setf (sicl-genv:variable-type name environment) type))))
        (otherwise
         (unless (member (car declaration-specifier) (sicl-genv:declarations environment))
           (error "Unknown proclamation ~a" declaration-specifier)))))

    (def cl:constantp (form &optional env)
      ;; expand?
      (unless env (setf env environment))
      (typecase form
        ((cons (eql quote) (cons t null)) t)
        (symbol (nth-value 1 (sicl-genv:constant-variable form env)))
        (t t)))
    ;;; TODO: packages?
    ))

;;; FIXME: this isn't really "basics" so much as a grab bag after the first few.
(defun install-basics (environment)
  (setf (sicl-genv:constant-variable 'sicl-genv:+global-environment+ environment) environment)
  ;; only used in the compile-file/loader stuff, and kind of dumbly
  (setf (sicl-genv:special-variable 'sicl-genv:*global-environment* environment t) environment)
  (setf (sicl-genv:fdefinition 'sicl-genv:global-environment environment)
        (lambda () environment))
  (macrolet ((copyf (name)
               `(setf (sicl-genv:fdefinition ',name environment) #',name))
             (copym (name)
               `(setf (sicl-genv:macro-function ',name environment) (macro-function ',name))))
    (copyf sicl-genv:macro-function)
    (copyf sicl-genv:function-cell)
    (copyf sicl-genv:variable-cell)
    (copyf sicl-genv:variable-unbound)
    #+clasp
    (copyf core:symbol-value-from-cell)
    #+clasp
    (copyf core:setf-symbol-value-from-cell)
    #+clasp
    (copyf core:multiple-value-funcall)
    #+(or)
    (progn
      ;; remf
      (copyf core:rem-f)
      ;; used in defmacro/destructuring-bind
      (copyf core::expand-defmacro) ; macro time
      (copyf core::dm-too-few-arguments) (copyf core::dm-too-many-arguments) ; runtime
      ;; backquote
      (copym core:backquote)
      (copyf core:backquote-append)
      (copyf core:backquote-append-list)
      ;; loop
      (copym core::with-loop-list-collection-head)
      (copym core::loop-collect-rplacd)
      (copym core::loop-collect-answer)
      (copym core::loop-copylist*)
      (copym core::loop-body)
      (copym core::loop-really-desetq)
      (copym core:cons-car) (copym core:cons-cdr)
      ;; do et al.
      (copym core::until) (copym core::while)
      ))
  (values))

(defun install-default-setf-expander (environment)
  ;; basically copied from SICL/Code/Compiler/Extrinsic-environment/define-default-setf-expander.lisp
  ;; not that there are many ways to do it.
  (setf (sicl-genv:default-setf-expander environment)
        (lambda (form env)
          (declare (ignore env))
          (let ((store (gensym "STORE")))
            (etypecase form
              (symbol
               (values nil nil (list store) `(setq ,form ,store) form))
              (cons
               (let ((temps (loop for arg in (rest form) collect (gensym))))
                 (values temps (rest form) (list store)
                         `(funcall #'(setf ,(first form)) ,store ,@temps)
                         `(,(first form) ,@temps)))))))))

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
    ensure-generic-function
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

;;; which macros are "safe" depends on their (implementation-defined) expansions.
;;; These are quite possibly "safe" in general though.
;;; but expansions may require implementation functions to run!
(defparameter *safe-macros*
  '(nth-value prog prog2 psetq case with-hash-table-iterator dotimes prog* loop with-open-stream
    with-standard-io-syntax with-accessors when prog1 call-method do do* and with-compilation-unit
    print-unreadable-object multiple-value-setq unless destructuring-bind with-slots cond
    multiple-value-bind dolist multiple-value-list loop-finish lambda time return with-output-to-string
    or ecase lambda))

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

(defun install-funcall-apply (environment)
  (setf (sicl-genv:fdefinition 'funcall environment)
        ;; FIXME: we could use a more basic apply that doesn't check designators or the type.
        ;; Also this is an awkward way to write.
        ;; Note the cl: are unnecessary, just intended to emphasize this is in no way recursive.
        (let ((coerce (sicl-genv:fdefinition 'coerce:fdesignator environment)))
          (lambda (fdesignator &rest arguments)
            (cl:apply (cl:funcall coerce fdesignator) arguments))))
  (setf (sicl-genv:compiler-macro-function 'funcall environment)
        (compiler-macro-lambda funcall (fdesignator &rest arguments)
                               `(cleavir-primop:funcall (coerce:fdesignator ,fdesignator) ,@arguments)))
  ;;; FIXME: Because we do not have an APPLY that does not coerce function designators, we cannot put
  ;;; it in the environment for APPLY calls to inline to. We could make our own, basically just apply but
  ;;; maybe with a check-type, but that wouldn't really help efficiency.
  (setf (sicl-genv:fdefinition 'apply environment)
        (let ((coerce (sicl-genv:fdefinition 'coerce:fdesignator environment)))
          (lambda (fdesignator &rest spreadable-arguments)
            (cl:apply #'cl:apply (cl:funcall coerce fdesignator) spreadable-arguments))))
  (values))

(defun import-setfs (environment)
  ;; note that (sicl-genv:setf-expander whatever nil) won't work until setf.lisp is loaded.
  (macrolet ((copy (name)
               `(setf (sicl-genv:setf-expander ',name environment)
                      (sicl-genv:setf-expander ',name nil))))
    (copy car) (copy cdr) (copy caar) (copy cadr) (copy cdar) (copy cddr)
    (copy caaar) (copy caadr) (copy cadar) (copy caddr) (copy cdaar) (copy cdadr) (copy cddar) (copy cdddr)
    (copy caaaar) (copy caaadr) (copy caadar) (copy caaddr)
    (copy cadaar) (copy cadadr) (copy caddar) (copy cadddr)
    (copy cdaaar) (copy cdaadr) (copy cdadar) (copy cdaddr)
    (copy cddaar) (copy cddadr) (copy cdddar) (copy cddddr))
  (values))

(defun install-system-construction (environment)
  ;;; FIXME: compile-file uses cl:read, so it's problematic
  (setf (sicl-genv:fdefinition 'compile-file environment)
        (lambda (input-file &rest keys &key &allow-other-keys)
          (let ((cmp:*cleavir-compile-file-hook* (sandbox-compile-file-hook environment)))
            (apply #'compile-file input-file keys))))
  (setf (sicl-genv:fdefinition 'core:load-binary environment) #'core:load-binary))

(defun install-variables (environment)
  (macrolet ((dvar (name &optional (value nil value-p))
               (if value-p
                   `(setf (sicl-genv:special-variable ',name environment t) ,value)
                   `(setf (sicl-genv:special-variable ',name environment nil) nil)))
             (dvars (&rest names)
               `(progn ,@(loop for name in names collecting `(dvar ,name)))))
    (dvars +++ ++ + /// // / *** ** * -)
    (dvar *features*))
  (values))

(defun install-constants (environment)
  (macrolet ((dconst (name value)
               `(setf (sicl-genv:constant-variable ,name environment) ,value)))
    (dconst t t) (dconst nil nil)
    (dconst pi pi)))

;;; KLUDGE: Honestly the whole thing. Clasp's reader is not flexible enough to do it right.
#+(or)
(defun install-reader (environment)
  (let ((standard-readtable (copy-readtable nil)))
    (labels ((force (fdesignator)
               (assert (not (null fdesignator)))
               (if (symbolp fdesignator) (symbol-function fdesignator) fdesignator))
             (copy (char)
               (multiple-value-bind (function non-terminating-p)
                   (get-macro-character char standard-readtable)
                 (set-macro-character char (force function) non-terminating-p standard-readtable)))
             (copyd (disp-char sub-char)
               (set-dispatch-macro-character
                disp-char sub-char
                (force (get-dispatch-macro-character disp-char sub-char standard-readtable))
                standard-readtable))
             (copys (sub-char)
               (copyd #\# sub-char)))
      ;; Skipping #. #s
      (copy #\() (copy #\)) (copy #\') (copy #\;) (copy #\") (copy #\`) (copy #\,)
      (copys #\\) (copys #\') (copys #\() (copys #\*) (copys #\:) (copys #\b)
      (copys #\B) (copys #\o) (copys #\O) (copys #\x) (copys #\X) (copys #\r)
      (copys #\R) (copys #\c) (copys #\C) (copys #\a) (copys #\A) (copys #\p)
      (copys #\P) (copys #\#) (copys #\=) (copys #\|) (copys #\<) (copys #\)))
    (set-dispatch-macro-character
     #\# #\.
     (lambda (stream subchar parameter)
       (declare (ignore subchar parameter))
       (let ((core::*backquote-level* 0))
         (let ((expr (read stream t nil t)))
           (cond (*read-suppress* nil)
                 (*read-eval* (eval-in-env expr environment))
                 (t #|FIXME|# (error 'reader-error :stream stream)))))))
    ;; FIXME: Currently clasp #s gets the constructor by looking in the system properties table,
    ;; which is not suitable for this. We'd rather want the constructor available in the class,
    ;; but this doesn't seem to be done...?

    ;; Anyway, here's where we actually install the danged thing
    ;; readtable itself is inaccessible and used as the standard readtable.
    ;; it's used as such in copy-readtable, set-syntax-from-char, and with-standard-io-syntax.
    (setf (sicl-genv:special-variable '*readtable* environment t)
          (copy-readtable readtable))
    (setf (sicl-genv:fdefinition 'copy-readtable environment)
          (lambda (&optional (from-readtable standard-readtable) to-readtable)
            (declare (core:lambda-name copy-readtable))
            (copy-readtable from-readtable to-readtable)))
    ;; KLUDGE: We don't copy most of the functions, because to make the reader work like this at
    ;; all, we need to rebind the reader variables, including *READTABLE*, thread locally.
    ;; This means that clasp's functions will refer to them properly.
    (setf (sicl-genv:fdefinition 'set-syntax-from-char environment)
          (lambda (to-char from-char
                   &optional (to-readtable *readtable*) (from-readtable standard-readtable))
            (declare (core:lambda-name set-syntax-from-char))
            (set-syntax-from-char to-char from-char to-readtable from-readtable)))
    ;; KLUDGE: The reader would do lookups in clasp's environment, so we can't allow symbols.
    (setf (sicl-genv:fdefinition 'set-dispatch-macro-character environment)
          (lambda (disp-char sub-char new-function &optional (readtable *readtable*))
            (declare (core:lambda-name set-dispatch-macro-character))
            (when (and (symbolp new-function) (not (null new-function)))
              (warn "In ~a: Sandbox environments do not yet support storing function designators
in readtables. Coercing ~a to function." 'set-dispatch-macro-character new-function)
              (setf new-function (sicl-genv:fdefinition new-function environment)))
            (set-dispatch-macro-character disp-char sub-char new-function readtable)))
    (setf (sicl-genv:fdefinition 'set-macro-character environment)
          (lambda (char new-function &optional non-terminating-p (readtable *readtable*))
            (declare (core:lambda-name set-macro-character))
            (when (and (symbolp new-function) (not (null new-function)))
              (warn "In ~a: Sandbox environments do not yet support storing function designators
in readtables. Coercing ~a to function." 'set-macro-character new-function)
              (setf new-function (sicl-genv:fdefinition new-function environment)))
            (set-macro-character char new-function non-terminating-p readtable)))

    ;; FIXME: we could actually skip this if the clasp macro bound (*readtable* (copy-readtable nil))
    (setf (sicl-genv:macro-function 'with-standard-io-syntax environment)
          (macro-lambda with-standard-io-syntax (&body body)
            `(let ((*package* (find-package "CL-USER"))
                   (*print-array* t)
                   (*print-base* 10)
                   (*print-case* :upcase)
                   (*print-circle* nil)
                   (*print-escape* t)
                   (*print-gensym* t)
                   (*print-length* nil)
                   (*print-level* nil)
                   (*print-lines* nil)
                   (*print-miser-width* nil)
                   (*print-pprint-dispatch* (core::copy-standard-pprint-dispatch-table))
                   (*print-pretty* nil)
                   (*print-radix* nil)
                   (*print-readably* t)
                   (*print-right-margin* nil)
                   (*read-base* 10)
                   (*read-default-float-format* 'single-float)
                   (*read-eval* t)
                   (*read-suppress* nil)
                   (*readtable* (copy-readtable nil)))
               ,@body)))
    ;; Helper function that returns the standard table (since it's not dumpable or readably printable)
    (setf (sicl-genv:fdefinition 'core::copy-standard-pprint-dispatch-table environment)
          (lambda ()
            (declare (core:lambda-name core::copy-standard-pprint-dispatch-table))
            ;; this is used in clasp's with-standard-io-syntax.
            ;; since the initial and standard tables are distinct, this is kind of weird,
            ;; but obscure anyway
            core::*initial-pprint-dispatch*)))
  (values))

(defun install-ensure-generic-function (environment)
  (let* ((egfuc
           (make-instance 'standard-generic-function
             :lambda-list '(gf-or-nil fname &key argument-precedence-order declarations documentation
                            generic-function-class lambda-list method-class method-combination name
                            &allow-other-keys)
             :name 'clos:ensure-generic-function-using-class))
         (method-class (clos:generic-function-method-class egfuc)))
    (multiple-value-bind (lambda initargs)
        (clos:make-method-lambda
         egfuc
         (clos:class-prototype method-class)
         '(lambda (gf-or-nil fname &rest keys
                   &key (generic-function-class (find-class 'standard-generic-function))
                   (method-class nil method-class-p)
                   (environment (sicl-genv:global-environment))
                   &allow-other-keys)
           (loop while (remf keys :generic-function-class))
           (loop while (remf keys :environment))
           (when (symbolp generic-function-class)
             (setf generic-function-class (sicl-genv:find-class generic-function-class environment)))
           (when (and method-class-p (symbolp method-class))
             (setf method-class (sicl-genv:find-class method-class environment)))
           (setf (sicl-genv:fdefinition fname environment)
            (if method-class-p
                (apply #'make-instance generic-function-class :method-class method-class :name fname keys)
                (apply #'make-instance generic-function-class :name fname keys))))
         environment)
      (add-method egfuc
                  (apply #'make-instance method-class
                         :function (cmp:compile-in-env nil lambda environment
                                                       cmp:*cleavir-compile-hook* 'llvm-sys:external-linkage)
                         :specializers (list (find-class 'null) (find-class 't))
                         :qualifiers '()
                         :lambda-list '(gf-or-nil fname &rest keys
                                        &key (generic-function-class (find-class 'standard-generic-function))
                                        (method-class nil method-class-p)
                                        (environment (sicl-genv:global-environment))
                                        &allow-other-keys)
                         initargs)))
    (setf (sicl-genv:fdefinition 'clos:ensure-generic-function-using-class environment)
          egfuc))
  (values))

(defun load-source (filespec &key verbose print if-does-not-exist external-format environment)
  (let* ((*readtable* *readtable*) (*package* *package*)
         (*load-pathname* (pathname (merge-pathnames filespec)))
         (*load-truename* (truename *load-pathname*))
         (sentinel (cons nil nil)))
    (with-open-file (s *load-truename* :external-format external-format :if-does-not-exist if-does-not-exist)
      (loop for form = (read s t sentinel) ; FIXME: make environment-sensitive
            until (eq form sentinel)
            do (eval-in-env form environment)))))

(defun import-classes (environment)
  (flet ((maybe-import (symbol)
           (let ((class (find-class symbol nil)))
             (when class
               (setf (sicl-genv:find-class symbol environment) class)))))
    (do-external-symbols (s "CL")   (maybe-import s))
    (do-external-symbols (s "CLOS") (maybe-import s)))
  (values))

(defun install-primitive-setfs (environment)
  (setf (sicl-genv:setf-expander 'macro-function environment)
        (macro-lambda (setf macro-function) (name)
          (let ((var (gensym)) (store (gensym)))
            (values (list var)
                    (list name)
                    (list store)
                    `(si:fset ,var ,store t)
                    `(macro-function ,var)))))
    (setf (sicl-genv:setf-expander 'fdefinition environment)
        (macro-lambda (setf fdefinition) (name)
          (let ((var (gensym)) (store (gensym)))
            (values (list var)
                    (list name)
                    (list store)
                    `(si:fset ,var ,store)
                    `(fdefinition ,var)))))
  (values))

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
  (install-funcall-apply environment)
  (install-multiple-value-call environment)
  (import-cl-functions environment *cl-environment*)
  (import-setfs environment)
  (install-variables environment)
  (install-constants environment)
  (import-classes environment)
  (values))
