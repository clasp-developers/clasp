(in-package #:clasp-sandbox)

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
      (warn "ignoring struct definition ~a" name))
    (def core:setf-compiler-macro-function (name function &optional env)
      ;; We don't seem to actually call this with an environment anywhere
      (when env (error "setf-compiler-macro-function called with non-NIL env ~a" env))
      (setf (sicl-genv:compiler-macro-function name cenv) function))
    (def clos::install-method-combination (name function)
      ;; let's cross these bridges when we come to them
      (warn "ignoring install-method-combination"))
    (def core:put-sysprop (key area value)
      (warn "Ignoring (put-sysprop ~a ~a ~a)" key area value))
    (def core:set-symbol-plist (sym list)
      (warn "ignoring set-symbol-plist"))
    (def clos:ensure-generic-function (name &rest arguments)
      (warn "ignoring ensure-generic-function"))
    (def clos:ensure-generic-function-using-class (&rest arguments)
      (warn "ignoring ensure-generic-function-using-class"))
    (def clos::install-method (&rest arguments)
      (warn "ignoring install-method"))
    (def clos::load-defclass (&rest arguments)
      (warn "ignoring load-defclass"))
    (def clos:ensure-class (&rest arguments)
      (warn "ignoring ensure-class"))
    (def clos:ensure-class-using-class (&rest arguments)
      (warn "ignoring ensure-class-using-class")))
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
