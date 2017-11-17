(in-package #:clasp-sandbox)

;;; DEFMETHOD tries to do various things with functions as actual generic functions,
;;; meaning putting just functions there results in errors. Therefore... This shit.
;;; I don't think either will be actually called, so let's go methodless.
;;; At compile time they're only needed for make-method-lambda, etc.
(defpackage #:sandbox-defs ; avoid shadowing crap
  (:export #:ensure-generic-function-using-class #:ensure-class-using-class))
(defgeneric sandbox-defs:ensure-generic-function-using-class
    (generic-function-or-nil function-name &rest keys))
(defgeneric sandbox-defs:ensure-class-using-class
    (class name &rest keys))

(defun initialize-evaluation-environment (cenv)
  ;; do this first so that later things can override definitions.
  ;; this is the eval environment, so we need everything, including actual definitions.
  (import-from-toplevel cenv)
  (install-basics cenv)
  (install-sandbox-accessors cenv)
  (install-default-setf-expander cenv)
  ;; Redirect everything that tries to modify the environment. (Hopefully.)
  (macrolet ((def (name lambda-list &body body)
               `(setf (sicl-genv:fdefinition ',name cenv)
                      (lambda ,lambda-list
                        (declare (core:lambda-name ,name))
                        ,@body))))
    (def core:fset (name function &optional macrop pretty-print lambda-list)
      (declare (ignore pretty-print lambda-list))
      (if macrop
          (setf (sicl-genv:macro-function name cenv) function)
          (setf (sicl-genv:fdefinition name cenv) function)))
    ;;; the compile time "hey, we defined a function" gizmo.
    (def cmp:register-global-function-def (context name)
      (when (eq context 'defun)
        (unless (cleavir-env:function-info (compilation-environment cenv) name) ; already known
          ;; this is how sicl makes functions known to the compiler.
          (setf (sicl-genv:function-type name (compilation-environment cenv))
                '(function * *))))
      ;; the actual one is just in place for warnings etc
      (cmp:register-global-function-def context name))
    (def core:*make-special (symbol)
      #+(or)
      (let ((*package* (find-package "KEYWORD")))
        (format t "Making ~a special~%" symbol))
      (setf (sicl-genv:special-variable symbol cenv nil) nil))
    (def core::register-global (symbol)
      ;; stupid alias for the same thing
      (setf (sicl-genv:special-variable symbol cenv nil) nil))
    (def core::define-structure (name conc-name type named slots slot-descriptions
                                      copier include print-function constructors offset name-offset
                                      documentation predicate)
      ;; for now just do nothing. we might not actually need most structures,
      ;; and if we do a defclass may be sufficient.
      (declare (ignore conc-name type named slots slot-descriptions copier
                       incude print-function constructors offset name-offset
                       documentation predicate))
      #-(or)
      (warn "ignoring struct definition ~a" name))
    (def core:setf-compiler-macro-function (name function &optional env)
      ;; We don't seem to actually call this with an environment anywhere
      (when env (error "setf-compiler-macro-function called with non-NIL env ~a" env))
      (setf (sicl-genv:compiler-macro-function name cenv) function))
    (def clos::install-method-combination (name function)
      ;; let's cross these bridges when we come to them
      #-(or)
      (warn "ignoring install-method-combination"))
    (def core::do-setf-method-expansion (name lambda args &optional stores-no)
      #-(or)
      (warn "ignoring do-setf-method-expansion"))
    (def core:put-sysprop (key area value)
      #-(or)
      (warn "Ignoring (put-sysprop ~a ~a ~a)" key area value))
    (def core:set-symbol-plist (sym list)
      #-(or)
      (warn "ignoring set-symbol-plist"))
    (def clos:ensure-generic-function (name &rest arguments)
      (let ((gf (sicl-genv:fdefinition name cenv)))
        (if (null gf) ; not typep generic-function, since e.g. this function is not a generic :(
            (error "ensure-generic-function for unknown gf ~a" name)
            gf)))
    (def clos::install-method (&rest arguments)
      #-(or)
      (warn "ignoring install-method"))
    (def clos::load-defclass (&rest arguments)
      #-(or)
      (warn "ignoring load-defclass"))
    (def clos:ensure-class (&rest arguments)
      #-(or)
      (warn "ignoring ensure-class")))
  (setf (sicl-genv:fdefinition 'clos:ensure-generic-function-using-class cenv)
        #'sandbox-defs:ensure-generic-function-using-class)
  (setf (sicl-genv:fdefinition 'clos:ensure-class-using-class cenv)
        #'sandbox-defs:ensure-class-using-class)
  (install-coerce-fdesignator cenv)
  (install-multiple-value-call cenv)
  ;; FIXME: move to a function
  (setf (sicl-genv:fdefinition 'declare-function cenv)
        (lambda (name lambda-list)
          (declare (core:lambda-name declare-function))
          (declare-function name lambda-list cenv)))
  (setf (transferringp cenv) t)
  (values))

(defun initialize-compiler-environment (lenv)
  (import-from-toplevel lenv t)
  ;; FIXME: move these
  (read-interpreter-info lenv #p"/tmp/interpreter-info.sexp")
  ;;(import-interpreter-knowns lenv)
  (initialize-evaluation-environment (compilation-environment:evaluation-environment lenv))
  (values))
